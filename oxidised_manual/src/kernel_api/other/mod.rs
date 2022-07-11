/// # Interupt Management
///
/// The system has an [IRQControl] capability that exists as a singleton inside the root-server. This is used to create [IRQHandler] capabilities, which are special forms of [Notification], that become the exclusive notification to an interrupt source. An `IRQHandler` can perform an acknowledgement, which signals to the kernel to re-enable the interrupt, and provide the next one that arrives.
///
///
/// ```C
/// // helper to set aside a capability slot
/// seL4_CPtr irq = cspace_alloc_slot(&cspace);
/// // create an IRQHandler, and place it into the slot.
/// seL4_error err = cspace_irq_control_get(&*cspace, irq, seL4_CapIRQControl,
///                                         irq_number, true);
/// // bind the interupt to an existing notification
/// seL4_IRQHandler_SetNotification(irq, ntfn);
/// // Unmask/activate an the interrupt.
/// seL4_IRQHandler_Ack(irq);
/// ````
///
/// # Device Drivers
///
/// 1. Handle interputs
/// 2. Communicate with rest of OS
/// 3. Access device registers.
///
/// ARM uses memory-mapped IO
///
/// * Needs to find frame-cap from bootinfo structure
/// * Provide the devices page to the driver by putting it into its VSpace
