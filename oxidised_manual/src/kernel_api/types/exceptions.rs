/// The kernel will send a message to the fault [EndPoint] when an exception is raised
///
/// For non-timeout exception
/// The message is configured to appear coming from the source of the fault.
/// Replying will restart the thread.
///
/// For timeout-exceptions
/// Has its own endpoint.
/// Is optional for a thread to have a timeout endpoint
/// without an endpoint, a timeout is silently ignored.
enum Exception {
    /// e.g. for instruction emulation, virtualisation, invalid code.
    InvalidSyscall,
    /// capability lookup failed, or invalid capbility
    CapabilityFault,
    /// e.g. address !mapped, invalid address.
    ///
    /// Possible handling might call for stack/heap growth, or load a dylib
    PageFault,
    /// e.g. div-by-zero, unaligned access, etc.
    Architecture,
    /// time budget fully consumed.
    Timeout,
}
