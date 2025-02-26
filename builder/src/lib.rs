use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, quote_spanned};
use syn::{spanned::Spanned, Data, DataStruct, Fields};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    // get base type name
    let base_type = &input.ident;

    // builer type name
    let builder_type = Ident::new(&format!("{}Builder", base_type), Span::call_site());

    // get struct fields
    let builder_fields = generate_builder_fields(&input);

    // initialize builder fields
    let builder_init = initialize_builder_fields(&input);

    // does it have named fields? If yes then create initializers for all of
    // them.
    let field_initializers = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => {
            let recurse = fields.named.iter().map(|f| {
                let ident = &f.ident;
                let ty = &f.ty;

                if let Some(inner_ty) = ident_is_option(ty) {
                    // in case of option type the initializer just needs
                    // the type inside Option
                    return quote_spanned! {f.span()=>
                        fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                            self.#ident = std::option::Option::Some(#ident);
                            self
                        }
                    };
                }

                if let Ok(Some(initializer_name)) = get_each_builder_name(f) {
                    // field is a vector with builder defined.

                    // get T inside Vec<T>
                    let inner_ty = get_vec_inner_type(ty);

                    // get builder name as speified inside the builder
                    let builder_name = syn::Ident::new(&initializer_name, Span::call_site());

                    let vec_builder = quote_spanned! {f.span()=>
                        fn #builder_name(&mut self, #ident: #inner_ty) -> &mut Self {
                            match self.#ident.take() {
                                std::option::Option::Some(mut vec) => {
                                    vec.push(#ident);
                                    self.#ident = std::option::Option::Some(vec);
                                }
                                std::option::Option::None => {
                                    self.#ident = std::option::Option::Some(std::vec![#ident]);
                                }
                            }
                            self
                        }
                    };

                    if &builder_name == ident.as_ref().unwrap() {
                        return quote_spanned!(f.span()=> #vec_builder);
                    } else {
                        return quote_spanned! {f.span()=>
                            #vec_builder

                            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                                self.#ident = std::option::Option::Some(#ident);
                                self
                            }
                        };
                    }
                } else if let Err(e) = get_each_builder_name(f) {
                    // wrong argument to 'builder' inert attribute
                    return e.into_compile_error();
                }

                // no Option or vec, return normal
                return quote_spanned! {f.span()=>
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                };
            });

            quote!(#(#recurse)*)
        }

        // otherwise set it to empty token stream
        _ => quote!(),
    };

    // empty token stream in case of tuple struct
    let build_function = generate_build_function(&input, &base_type);

    quote!(
        impl #base_type {
            pub fn builder() -> #builder_type {
                #builder_type #builder_init
            }
        }

        impl #builder_type {
            #field_initializers

            #build_function
        }

        pub struct #builder_type #builder_fields
    )
    .into()
}

// If the named field has builder attribute then get its builder function name
// in Some. If it succeeds then the field is assumed to be a vector for
// further processing
fn get_each_builder_name(field: &syn::Field) -> Result<Option<String>, syn::Error> {
    // match to make sure #[builder(...)] is available, then parse the inner
    // tokenstream to get 'each' function name
    for attr in field.attrs.iter() {
        match &attr.meta {
            syn::Meta::List(syn::MetaList { ref path, .. }) if path.is_ident("builder") => {
                // parse the tokenstream inside List
                let builder: syn::Expr = attr.parse_args()?;

                if let syn::Expr::Assign(syn::ExprAssign { left, right, .. }) = builder {
                    // check if left is correct 'each'
                    match *left {
                        syn::Expr::Path(syn::ExprPath { ref path, .. })
                            if !path.is_ident("each") =>
                        {
                            println!("wrong builder arg");
                            return Err(syn::Error::new(left.span(), "expected 'each'"));
                        }
                        _ => {}
                    }

                    if let syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(ref lit_str),
                        ..
                    }) = *right
                    {
                        return Ok(Some(lit_str.value()));
                    }
                }
            }
            _ => {}
        }
    }

    Ok(None)
}

// Must be a vec for it to work.
fn get_vec_inner_type(ty: &syn::Type) -> &syn::Type {
    let mut segments = if let syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path { segments, .. },
    }) = ty
    {
        segments.iter()
    } else {
        unreachable!();
    };

    let mut args = if let Some(syn::PathSegment {
        ident,
        arguments:
            syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
    }) = segments.next()
    {
        assert_eq!(ident.to_string(), "Vec");
        args.iter()
    } else {
        unreachable!();
    };

    // get inner type of Option
    let inner_type = match args.next() {
        Some(syn::GenericArgument::Type(ref inner_type)) => inner_type,
        _ => unreachable!(),
    };

    assert!(
        args.next().is_none(),
        "There is supposed to be only one type inside Vec"
    );

    inner_type
}

// If ty is Option<T> then returns type T in Some, otherwise None
fn ident_is_option(ty: &syn::Type) -> Option<&syn::Type> {
    let mut segments = if let syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path { segments, .. },
    }) = ty
    {
        segments.iter()
    } else {
        return None;
    };

    let mut args = if let Some(syn::PathSegment {
        ident,
        arguments:
            syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
    }) = segments.next()
    {
        if ident.to_string() != "Option" {
            return None;
        }

        args.iter()
    } else {
        return None;
    };

    // get inner type of Option
    let inner_type = match args.next() {
        Some(syn::GenericArgument::Type(ref inner_type)) => inner_type,
        _ => return None,
    };

    assert!(
        args.next().is_none(),
        "There is supposed to be only one type inside Option"
    );

    Some(inner_type)
}

fn generate_build_function(
    input: &syn::DeriveInput,
    base_type: &syn::Ident,
) -> proc_macro2::TokenStream {
    match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => {
            let recurse_creation = fields.named.iter().map(|f| {
                let ident = &f.ident;

                // its not a tuple struct so unwrap is ok
                let ident_name = ident.as_ref().unwrap().to_string();
                if ident_is_option(&f.ty).is_some() {
                    quote_spanned!(f.span()=>
                        let #ident = self.#ident.take().or(std::option::Option::None);
                    )
                } else {
                    quote_spanned!(f.span()=>
                        let #ident = self.#ident.take().ok_or(
                            std::format!("field '{}' is not set.", #ident_name)
                        )?;
                    )
                }
            });

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

// Create builder fields struct. Each field is just Option<T> where T is the
// type of corresponding original field.
fn generate_builder_fields(input: &syn::DeriveInput) -> proc_macro2::TokenStream {
    match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => {
            let recurse = fields.named.iter().map(|f| {
                let ident = &f.ident;
                let ident_type = &f.ty;
                match ident_is_option(ident_type) {
                    Some(inner_type) => {
                        quote_spanned!(f.span()=> #ident: std::option::Option<#inner_type>)
                    }
                    None => {
                        quote_spanned!(f.span()=> #ident: std::option::Option<#ident_type>)
                    }
                }
            });
            quote!({#(#recurse),*})
        }

        Data::Struct(DataStruct {
            fields: Fields::Unit,
            ..
        }) => {
            unimplemented!("no need to create builder for unit struct");
        }

        _ => unimplemented!(),
    }
}

// Initialize the fields of builder struct
fn initialize_builder_fields(input: &syn::DeriveInput) -> proc_macro2::TokenStream {
    match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => {
            let recurse = fields.named.iter().map(|f| {
                let ident = &f.ident;
                quote_spanned!(f.span()=> #ident: std::option::Option::None)
            });
            quote!({#(#recurse),*})
        }

        Data::Struct(DataStruct {
            fields: Fields::Unit,
            ..
        }) => {
            unimplemented!("no need to create builder for unit struct");
        }

        _ => unimplemented!(),
    }
}
