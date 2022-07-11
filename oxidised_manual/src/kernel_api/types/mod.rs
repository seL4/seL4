pub mod capabilities;
// I'm pretty happy with this
pub mod err;
pub mod message;
mod primitives;

pub use primitives::{Badge};

pub use err::SeL4Error;


impl !Send for CapPtr {}
impl !Sync for CapPtr {}
pub struct CapPtr(usize);

