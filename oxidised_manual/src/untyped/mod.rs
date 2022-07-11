struct Untyped {}

impl Untyped {
    // TODO maybe do a set of `impl TryFrom<Untyped> for Object`??
    fn retype(
        self,
        obj_type: Object,
        as_size: Word,
        root: &mut CapSpaceNode,
        slot: Slot,
        slot_count: Word,
        obj_count: Word,
    ) {}


}
