use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{spanned::Spanned, Data, DataStruct, Fields};

// Checks if its a named struct
fn is_named_struct(data: &syn::Data) -> bool {
    matches!(data, syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named( _), ..
    }))
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    // get base type name
    let base_type = &input.ident;

    // builer type name
    let builder_type = Ident::new(&format!("{}Builder", base_type), Span::call_site());

    // check if its a named field struct, if not then return with message
    if !is_named_struct(&input.data) {
        return syn::Error::new(
            base_type.span(),
            "must be a struct with named fields"
        ).to_compile_error().into();
    }

    // get fields of the builder struct.
    let builder_fields = get_builder_struct_fields(&input);
    // println!("{builder_fields:#?}");

    // initialize them.
    let builder_init = initialize_builder_fields(&input);

    let field_initializers = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => {
            let recurse = fields.named.iter().map(|f| {
                match create_initializers(f) {
                    Ok(t) => t,
                    Err(e) => e,
                }
            });

            quote!(#(#recurse)*)
        }

        // otherwise set it to empty token stream
        _ => unreachable!(),
    };

    // empty token stream in case of tuple struct
    let build_function = generate_build_function(&input, &base_type);

    quote!(
        impl #base_type {
            pub fn builder() -> #builder_type {
                #builder_type #builder_init
            }
        }

        pub struct #builder_type #builder_fields

        impl #builder_type {
            #field_initializers

            #build_function
        }

    )
    .into()
}

// Initializer functions for builder struct based on types of fields in the
// original struct. Option types just accept the inner type as input. Vec types
// also need just the inner type.  Handle 'each' builders as well.
fn create_initializers(field: &syn::Field) -> Result<TokenStream, TokenStream> {
    let field_ident = &field.ident;

    // field has single builder
    match get_single_builder_name(field) {
        Ok(Some(single_builder_name)) => {
            // Create single builder ident
            let single_builder = Ident::new(&single_builder_name, Span::call_site());
            
            // get the inner type of vector
            let inner_t = is_same_container_type(&field.ty, "Vec").unwrap();

            // if conflict happens in single and bulk builder names then prefer 
            // the single builder only, otherwise create both builder functions.
            if single_builder_name == field.ident.as_ref().unwrap().to_string() {
                Ok(quote_spanned!{field.span()=> 
                    fn #single_builder(&mut self, #single_builder: #inner_t) -> &mut Self {
                        self.#field_ident.push(#single_builder);
                        self
                    }
                })
            } else {
                Ok(quote_spanned!{field.span()=> 
                    fn #single_builder(&mut self, #single_builder: #inner_t) -> &mut Self {
                        self.#field_ident.push(#single_builder);
                        self
                    }

                    fn #field_ident(&mut self, #field_ident: std::vec::Vec<#inner_t>) -> &mut Self {
                        self.#field_ident = #field_ident;
                        self
                    }
                })
            }
        }

        Ok(None) => {
            // not a vec with single builder. But still could be a vec, or an 
            // Option.
            let t = &field.ty;
            if let Some(_) = is_same_container_type(&field.ty, "Vec") {
                // take entire vec as input
                Ok(quote_spanned!{field.span()=> 
                    fn #field_ident(&mut self, #field_ident: #t) -> &mut Self {
                        self.#field_ident = #field_ident;
                        self
                    }
                })
            } else if let Some(inner_t) = is_same_container_type(&field.ty, "Option") {
                // take type inside Option as input
                Ok(quote_spanned!{field.span()=> 
                    fn #field_ident(&mut self, #field_ident: #inner_t) -> &mut Self {
                        self.#field_ident = Some(#field_ident);
                        self
                    }
                })
            }
            else {
                Ok(quote_spanned!{field.span()=> 
                    fn #field_ident(&mut self, #field_ident: #t) -> &mut Self {
                        self.#field_ident = Some(#field_ident);
                        self
                    }
                })
            }
        }

        // wrong inert attribute specified
        Err(e) => {
            Err(e.to_compile_error())
        }
    }
}

// If the named field has builder attribute then get its builder function name
// in Some. Return Err if error happens in parsing #[builder(each = "...")]
fn get_single_builder_name(field: &syn::Field) -> Result<Option<String>, syn::Error> {
    // match to make sure #[builder(...)] is available, then parse the inner
    // tokenstream to get 'each' function name
    for attr in field.attrs.iter() {
        // check if its #[builder(...)]
        if let syn::Meta::List(syn::MetaList {
            ref path, .. 
        }) = &attr.meta {
            if !path.is_ident("builder") {
                continue;
            }
            // parse the tokenstream inside List
            let builder: syn::Expr = attr.parse_args()?;

            if let syn::Expr::Assign(syn::ExprAssign { left, right, .. }) = builder {
                // check if left is 'each'
                if let syn::Expr::Path(syn::ExprPath { ref path, .. }) = *left {
                    if !path.is_ident("each") {
                        return Err(syn::Error::new(left.span(), "expected 'each'"));
                    }
                }

                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(ref lit_str),
                    ..
                }) = *right {
                    return Ok(Some(lit_str.value()));
                }
            }
        }
    }
    Ok(None)
}

// If ty is the given container type then return the inner type in Option. 
// Useful for getting innter types of Option and Vec.
fn is_same_container_type<'a>(ty: &'a syn::Type, container_ty: &'static str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path { segments, .. },
    }) = ty {
        match segments.iter().next() {
            Some(syn::PathSegment {
                ref ident,
                arguments: syn::PathArguments::AngleBracketed(
                    syn::AngleBracketedGenericArguments {
                        ref args, .. 
                    }
                )}) if ident.to_string() == container_ty => {
                if let Some(syn::GenericArgument::Type(ref inner_type)) = args.iter().next() {
                    Some(inner_type)
                } else {
                    None
                }
            }

            _ => None,
        }
    } else { None }
}

// The build() function on the xBuilder struct
fn generate_build_function(input: &syn::DeriveInput, base_type: &syn::Ident) -> TokenStream {
    match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => {
            // create individual fields with the same name as the field name in 
            // struct
            let recurse_creation = fields.named.iter().map(|f| {
                let ident = &f.ident;

                // its not a tuple struct so unwrap is ok
                let ident_name = ident.as_ref().unwrap().to_string();
                if is_same_container_type(&f.ty, "Option").is_some() {
                    quote_spanned!{f.span()=>
                        let #ident = self.#ident.clone();
                    }
                } else if is_same_container_type(&f.ty, "Vec").is_some() {
                    quote_spanned!{f.span()=> 
                        let #ident = self.#ident.clone();
                    }
                } else {
                    quote_spanned!(f.span()=>
                        let #ident = self.#ident.clone().ok_or(
                            std::format!("field '{}' is not set.", #ident_name)
                        )?;
                    )
                }
            });

            // get individual field names.
            let recurse_ident = fields.named.iter().map(|f| {
                let ident = &f.ident;
                quote_spanned!(f.span()=> #ident)
            });

            quote! {
                fn build(&mut self) -> std::result::Result<#base_type, std::boxed::Box<dyn std::error::Error>> {
                    #(#recurse_creation)*

                    std::result::Result::Ok(#base_type {
                        #(#recurse_ident),*
                    })
                }
            }
        }
        _ => quote!(),
    }
}

// Creates builder struct fields. Each field is just Option<T> where T is the
// type of corresponding original field. If original field is already Option 
// then don't wrap it again in another Option. If the type is a Vec<T> then 
// leave builder field type to be same as well.
fn get_builder_struct_fields(input: &syn::DeriveInput) -> TokenStream {
    match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => {
            let recurse = fields.named.iter().map(|f| {
                let ident_type = &f.ty;
                let ident = &f.ident;
                if let Some(inside_option) = is_same_container_type(ident_type, "Option") {
                    quote_spanned!(f.span()=> #ident: std::option::Option<#inside_option>)
                } else if let Some(inside_vec) = is_same_container_type(ident_type, "Vec") {
                    quote_spanned!(f.span()=> #ident: std::vec::Vec<#inside_vec>)
                } else {
                    quote_spanned!(f.span()=> #ident: std::option::Option<#ident_type>)
                }
            });
            quote!({#(#recurse),*})
        }
        _ => unreachable!("unreachable in named struct type"),
    }
}

// Initialize the fields of builder struct. Field is either Option or Vec. Both
// has defaults.
fn initialize_builder_fields(input: &syn::DeriveInput) -> TokenStream {
    match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => {
            let recurse = fields.named.iter().map(|f| {
                let ident = &f.ident;
                quote_spanned!(f.span()=> #ident: std::default::Default::default())
            });
            quote!({#(#recurse),*})
        }
        _ => unimplemented!(),
    }
}
