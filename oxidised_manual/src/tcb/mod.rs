pub struct TCB {}

#[derive(Default)]
struct TCBConfig {
    fault_ep: Option<CapPtr>,
    capspace_root: Option<CapSpaceNode>,
    root_guard: Option<Guard>,
    vspace_root: Option<CapPtr>,
    // vspace_root_data: Option<CapPtr>, // no effect on x86 or ARM
    /// 512-byte alligned address for threads IPC buffer
    /// Cannot cross page boundary
    buffer: Option<Word>,
    /// Capability to the page containing the threads IPC buffer
    buffer_frame: Option<CapPtr>,
}
enum Todo {
    Msg(&'static str),
}
// pretty sure the builder pattern is useless. You config a TCB already
// created...
// leaving here for now as reference
impl TCBConfig {
    fn new() -> Self {
        Self::default()
    }

    /// The CSpace node that serves as the root node for
    /// the TCB to be created
    fn set_cspace_root(self, val: CapSpaceNode) -> Result<Self, Todo> {
        if val.is_some() {return Err(Todo::Msg("already set"))}
        self.capspace_root = Some(val);
        self
    }
    fn set_cspace_root_data(self, val: Guard) -> Result<Self, Todo> {
        if val.is_some() {return Err(Todo::Msg("already set"))}
        self.cspace_root_data = some(val);
        self
    }

    /// will become the new vspace root
    fn set_vspace_root(self, val: CapPtr) -> Result<Self, Todo> {
        if val.is_some() {return Err(Todo::Msg("already set"))}
        self.vspace_root = some(val);
        self
    }

    /// Checks to make sure that the buffer is 512-byte alligned, doesn't cross
    /// page boundaries, and is inside the page delegated to hold the buffer.
    fn set_buffer_and_frame(self, buf: Word, frm: CapPtr) -> Result<Self, (Self, Todo)> {
        if self.buffer.is_some() {return Err((self, Todo::Msg("already set")))}
        if buf % 512 != 0 {return Err((self, Msg("must be 512-byte aligned")))}
        if buf / 4096 + core::mem::size_of::<IPCBuffer>() > 4095 {
            return Err((self, Todo::Msg("the buffer cannot cross page boundary")));
        }
        // TODO add checks to make sure the ipc buffer is in frame
        self.buffer = some(buf);
        self.buffer_frame = Some(frm);
    }

    fn build(self) -> Result<TCB, Self> {
        if self.fault_ep.is_none()
            || self.cspace_root.is_none()
            || self.vspace_root.is_none()
            || self.buffer.is_none()
            || self.buffer_frame.is_none()
        {
            return Err(self)
        }
        unimplemented!();
    }
}
impl TCB {
    fn bind_ntfn(&mut self, ntfn: CapPtr) {} 

    /// Set the parameters of the TCB
    fn config(
        &mut self,
        fault_ep: CapPtr,
        cspace_root: CapSpaceNode,
        cspace_root_guard: Option<Guard>,
        vspace_root: VSpace,
        buffer: Word,
        buffer_fram: CapPtr,
    ) {}
    fn set_ipc_buffer(
        &mut self,
        buffer: IPCBuffer,
        frame: CapPtr,
    ) {}

    fn set_max_controlled_priority(
        &mut self,
        authority: &Self,
        mcp: Word,
    ) {}
    fn set_priority(
        &mut self,
        authority: &Self,
        priority: Word,
    ) {}
    fn set_sched_params(
        &mut self,
        authority: &Self,
        mcp: Word,
        priority: Word,
    ) {}
    fn set_space(
        &mut self,
        fault_ep: CapPtr,
        cspace_root: CapSpaceNode,
        cspace_root_guard: Option<Guard>,
        vspace_root: VSpace,
    ) {}
    fn suspend(&mut self) {}
    fn unbind_notification(&mut self) {}

    // TODO
    // fn set_thrd_local_stg()
    // fn copy_registers(){}
    // fn write_registers(){}
    // fn get_breakpoint(){}
    // fn set_breakpoint(){}
    // fn unset_breakpoint(){}
    // fn set_affinity(){}
    // fn resume(){}
}

struct ThreadLocalStorage {}
