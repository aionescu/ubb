use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{AttributeArgs, ItemFn};

#[proc_macro_attribute]
pub fn timed(args: TokenStream, input: TokenStream) -> TokenStream {
  if cfg!(not(feature = "enabled")) {
    return input
  }

  let args = syn::parse_macro_input!(args as AttributeArgs);
  let fun = syn::parse_macro_input!(input as ItemFn);

  let ItemFn { attrs, vis, sig, block, .. } = &fun;

  let name =
    args.first()
    .map(ToTokens::into_token_stream)
    .unwrap_or_else(|| sig.ident.to_string().into_token_stream());

  let code = quote! {
    #(#attrs)*
    #vis #sig {
      let mut body = move || #block;

      let time = std::time::Instant::now();
      let r = body();
      let elapsed = time.elapsed();

      println!("{} took {:?}.", #name, elapsed);
      r
    }
  };

  code.into()
}
