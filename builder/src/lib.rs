use proc_macro::TokenStream;
use quote::{quote, format_ident};
use syn::{parse_macro_input, DeriveInput, Data::{self, Struct}, Fields, Ident,
    PathSegment, PathArguments, AngleBracketedGenericArguments, GenericArgument,
    TypePath, Type, Path, spanned::Spanned,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!{input as DeriveInput};

    let builder_name = format_ident!("{}Builder", &input.ident.to_string());

    let base_name = input.ident;

    // derive Builder only on structs for now.
    assert!(matches!(input.data, Data::Struct {..}));

    let builder_fields_declaration = match &input.data {
        Struct(data_struct) => {
            match &data_struct.fields {
                Fields::Named(fields) => {
                    let recurse_declaration = fields.named.iter().map(|f| {
                        let name = &f.ident;
                        let ty = &f.ty;
                        quote! {
                            #name : std::option::Option<#ty>,
                        }
                    });

                    quote! {#(#recurse_declaration)* }
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    };

    let builder_fields_definition_stream = match &input.data {
        Struct(data_struct) => {
            match &data_struct.fields {
                Fields::Named(fields) => {
                    // part06: if field is vector of something then in definition the option should
                    // be an empty vector of that thing.
                    fields.named.iter().map(|f| {
                        // filter list attributes with path 'builder'
                        let attrs = f.attrs.iter().filter(|attr| {
                            if let syn::Meta::List(syn::MetaList {path, ..}) = &attr.meta {
                                if path.is_ident("builder") {
                                    true
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        }).collect::<Vec<_>>();

                        let one_by_one_setter = one_by_one_setter(&attrs)?;

                        if one_by_one_setter.is_some() {
                            let name = &f.ident;
                            Ok(quote! {
                                #name: Some(std::vec::Vec::new()),
                            })
                        } else {
                            let name = &f.ident;
                            Ok(quote! {
                                 #name : std::option::Option::None,
                            })
                        }

                    }).collect::<syn::Result<proc_macro2::TokenStream>>()
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    };

    let builder_fields_definition = match builder_fields_definition_stream {
        Ok(stream) => stream,
        Err(e) => return syn::Error::into_compile_error(e).into(),
    };

    // tokenstream of combined code for builder setter functions.
    let builder_setter_functions_stream = match &input.data {
        Struct(data_struct) => {
            match &data_struct.fields {
                Fields::Named(fields) => {
                    fields.named.iter().map(|f| {
                        // filter list attributes with path 'builder'
                        let attrs = f.attrs.iter().filter(|attr| {
                            if let syn::Meta::List(syn::MetaList {path, ..}) = &attr.meta {
                                if path.is_ident("builder") {
                                    true
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        }).collect::<Vec<_>>();

                        let one_by_one_setter = one_by_one_setter(&attrs)?;
                        let setter_name = &f.ident;
                        let arg_ty = &f.ty;
                        if let Some(one_by_one) = one_by_one_setter {
                            let mut stream = proc_macro2::TokenStream::new();
                            // the field is a vector of something. Get that something.

                            let vec_inner_type = if let syn::Type::Path(TypePath {path: syn::Path {segments, ..}, ..}) = arg_ty {
                                if let syn::PathSegment {arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, ..}), ..} = &segments[0] {
                                    match &args[0] {
                                        syn::GenericArgument::Type(ty) => ty,
                                        _ => unreachable!("wrong vec inner type"),
                                    }
                                } else {
                                    unreachable!("foo");
                                }
                            } else {
                                unreachable!("bar");
                            };

                            stream.extend(vec![quote! { 
                                fn #one_by_one(&mut self, arg: #vec_inner_type) -> &mut Self {
                                    self.#setter_name.get_or_insert_with(std::vec::Vec::new).push(arg);
                                    self
                                }
                            }]);

                            // conditionally generate all-at-once-builder
                            if setter_name.as_ref().unwrap() != &one_by_one {
                                stream.extend(vec![quote! { 
                                    fn #setter_name(&mut self, arg: #arg_ty) -> &mut Self {
                                        self.#setter_name = std::option::Option::Some(arg);
                                        self
                                    }
                                }]);

                            }

                            Ok(stream)
                        } else {
                            match is_type_option_of_something(arg_ty) {
                                Some(ty) => Ok(quote! {
                                    fn #setter_name(&mut self, arg: #ty) -> &mut Self {
                                        self.#setter_name = std::option::Option::Some(std::option::Option::Some(arg));
                                        self
                                    }
                                }),
                                None => Ok(quote! {
                                    fn #setter_name(&mut self, arg: #arg_ty) -> &mut Self {
                                        self.#setter_name = std::option::Option::Some(arg);
                                        self
                                    }
                                }),
                            }
                        }
                    }).collect::<syn::Result<proc_macro2::TokenStream>>()
                }
                Fields::Unnamed(_fields) => {
                    unimplemented!();
                }
                Fields::Unit => {
                    unimplemented!();
                }
            }
        }
        _ => unreachable!(),
    };

    let builder_setter_functions = match builder_setter_functions_stream {
        Ok(stream) => stream,
        Err(e) => return syn::Error::into_compile_error(e).into(),
    };

    let reverse_builder = generate_reverse_builder(&input.data, &base_name);

    TokenStream::from(quote!{
        pub struct #builder_name {
            #builder_fields_declaration
        }

        impl #base_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #builder_fields_definition
                }
            }
        }

        impl #builder_name {
            #builder_setter_functions

            #reverse_builder
        }

    })
}

// not usable when more than one atributes are given
fn one_by_one_setter(attrs: &[&syn::Attribute]) -> syn::Result<Option<Ident>> {
    match attrs.len() {
        0 => Ok(None),
        1 => {
            let expr_assign: syn::ExprAssign = match &attrs[0].parse_args()? {
                syn::Expr::Assign(expr_assign) => expr_assign.clone(),
                _ => todo!(),
            };

            if let syn::Expr::Path(syn::ExprPath { path, ..}) = *expr_assign.left {
                if path.is_ident("each") {
                    if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(litstr), ..}) = *expr_assign.right {
                        Ok(Some(syn::Ident::new(&litstr.value(), proc_macro2::Span::call_site())))
                    } else {
                        unreachable!("expected string literal for name");
                    }
                } else {
                    return Err(syn::Error::new(path.span(), "expected `builder(each = \"...\")`"))
                    // unreachable!("path == name, expected");
                }
            } else {
                unreachable!("left expression must be a path");
            }

            // panic!("expr_assign, right: {:?}, left: {:?}", expr_assign.right, expr_assign.left);
        }
        _ => {
            panic!("multiple attributes given where only one was expected");
        }
    }
}

fn is_type_option_of_something(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(
            TypePath {
                qself: None,
                path: Path {
                    segments, .. // leading_colon
                },
            },
        ) => {
            match segments.iter().next() {
                Some(path_segment) => {
                    match path_segment {
                        PathSegment {
                            ident,
                            arguments: PathArguments::AngleBracketed(
                                         AngleBracketedGenericArguments {
                                             args,
                                             ..
                                         },
                                     )
                            } => {
                                if ident.to_string() == "Option" {
                                    match args.iter().next() {
                                        Some(gen_arg) => {
                                            match gen_arg {
                                                GenericArgument::Type(ty) => {
                                                    Some(ty.clone())
                                                }
                                                _ => {
                                                    None
                                                }
                                            }
                                        }
                                        None => {
                                            None
                                        }
                                    }
                                } else {
                                    None
                                }
                            }
                        _ => {
                            None
                        }
                    }
                }
                None => {
                    None
                }
            }
        }
        _ => {
            None
        }
    }
}

fn generate_reverse_builder(data: &Data, base_name: &Ident) -> proc_macro2::TokenStream {
    match &data {
        Data::Struct(ref data) => {
            match &data.fields {
                Fields::Named(fields) => {
                    let recurse = fields.named.iter().map(|f| {
                        let name = &f.ident;
                        let field_ty = &f.ty;
                        match is_type_option_of_something(field_ty) {
                            Some(_) => quote! {
                                #name: match &self.#name {
                                    std::option::Option::Some(val) => val.clone(),
                                    std::option::Option::None => std::option::Option::None,
                                },
                            },
                            None =>  quote! {
                                #name: match &self.#name {
                                    std::option::Option::Some(val) => val.clone(),
                                    std::option::Option::None => return std::result::Result::Err(String::from("test").into()),
                                },
                            },
                        }
                    });

                    quote! {
                        pub fn build(&mut self) -> std::result::Result<#base_name, std::boxed::Box<dyn std::error::Error>> {
                            Ok(#base_name {
                                #(#recurse)*
                            })
                        }
                    }
                }
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}
