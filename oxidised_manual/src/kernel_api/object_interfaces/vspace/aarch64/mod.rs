//! Representing the aarch64 address management
//!
//! To illustrate the page-table structure:
//! ```
//! struct VSpaceWrapper {
//!     // A sparse-array of the first indirection
//!     inner: [Option<PageUpperDirWrapper> ; 512]
//! }
//! struct PageUpperDirWrapper {
//!     // A sparse-array of the second indirection
//!     inner: [Option<Level2> ; 512]
//! }
//! enum Level2 {
//!     // Another indirection to follow
//!     PageDirectory,
//!     // A mapping to 1GiB of physical memory
//!     // First indirection yielded a mapping to a physical frame
//!     HugePage,
//! }
//! struct PageDirWrapper {
//!     // A sparse-array of the third indirection
//!     inner: [Option<Level3> ; 512]
//! }
//! enum Level3 {
//!     PageTable,
//!     // A mapping to 2MiB of physical memory
//!     LargePage,
//! }
//! struct PageTabWrapper {
//!     // A sparse array of 4KiB mappings, and final indirection
//!     inner: [Option<SmallPage> ; 512]
//! }
//! ```
//! A mockup to visualise the aarch64 paging structure
//! ```text
//! The following address ranges are mapped in:
//! 0xDEADBEEF000..=0xDEADBEEFFFF (4KiB)
//! 0xDEADC400000..=0xDEADC5FFFFF (2MiB)
//! 0xDEB40000000..=0xDEB6FFFFFFF (1GiB)
//!
//!       000011011___110101011___011011111___011011111___0100_0010_0000
//!         │             │             │             │              │
//!  VSpace ▼      PgUpDir▼      PgDir  ▼      PgTable▼              │
//! ┌─────────┐ ┌─▲─────────┐ ┌─▲─────────┐ ┌─▲─────────┐            │
//! │000000000│ │ │000000000│ │ │000000000│ │ │000000000│            │
//! │.        │ │ │.        │ │ │.        │ │ │.        │            │
//! │.        │ │ │.        │ │ │.        │ │ │.        │            │
//! │.        │ │ │.        │ │ │.        │ │ │.        │            │
//! │.        │ │ │.        │ │ │011011111├─┘ │.        │            │
//! │.        │ │ │110101011├─┘ │.        │   │.        │  SmallPgObj▼
//! │000011011├─┘ │.        │   │.        │   │011101111├─▲────────────┐
//! │.        │   │110101101├─┐ │011100010├─┐ │.        │ │000000000000│
//! │.        │   │.        │ │ │.        │ │ │.        │ │            │
//! │.        │   │.        │ │ │.        │ │ │.        │ │010000100000│
//! │111111111│   │111111111│ │ │111111111│ │ │111111111│ │            │
//!  ▲ ───── ▲     ▲ ───── ▲  │  ▲ ───── ▲  │  ▲ ───── ▲  │111111111111│
//!  47     39     38     30  │  29     21  │  20     12   ▲ ──────── ▲
//!                           │             │              11         0
//!      HugePageObject       │             │  LargePageObject
//!     ┌─────────────────────▼────────┐    └─▲─────────────────────┐
//!     │000000000000000000000000000000│      │000000000000000000000│
//!     │..                            │      │.                    │
//!     │011100010011011111010000100000│      │011011111010000100000│
//!     │..                            │      │..                   │
//!     │111111111111111111111111111111│      │111111111111111111111│
//!      ▲ ────────────────────────── ▲        ▲ ───────────────── ▲
//!      29                           0        20                  0
//!```
mod paging;
pub use paging::{
    VSpace,
    PageUpperDir,
    PageDir,
    PageTable,
    Page,
    Mapping,
    CacheControl,
    IOPageTable,
};
