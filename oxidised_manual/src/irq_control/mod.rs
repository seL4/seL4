#![allow(unused_variables, dead_code)]
use crate::types::CapPtr;
use crate::types::capabilities::{IRQControl, IRQHandler, CapNode };

impl IRQControl {
    fn create_handler(&self, irq: usize, root: &mut CapNode, idx: CapPtr, depth: u8) {}
}
impl IRQHandler {
    fn ack(&self) {}
    fn clear(handler: IRQHandler) {}
    fn set_notification(&mut self, ntfn: CapPtr) {}
}
