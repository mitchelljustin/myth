use std::alloc::{GlobalAlloc, Layout};
use std::mem;

static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[repr(C)]
pub struct RefBox {
    strong: usize,
    data_size: usize,
}

fn refbox_layout_for_data_size(data_size: usize) -> Layout {
    let size = mem::size_of::<RefBox>() + data_size;
    let align = mem::align_of::<usize>();
    unsafe { Layout::from_size_align_unchecked(size, align) }
}

#[no_mangle]
pub fn new(data_size: usize) -> *mut RefBox {
    unsafe {
        let ptr = ALLOC.alloc(refbox_layout_for_data_size(data_size));
        ptr as *mut RefBox
    }
}

#[no_mangle]
pub fn inc_ref(refbox: *mut RefBox) {
    unsafe {
        (*refbox).strong += 1;
    }
}

#[no_mangle]
pub fn dec_ref(refbox: *mut RefBox) {
    unsafe {
        (*refbox).strong -= 1;
        if (*refbox).strong == 0 {
            let layout = refbox_layout_for_data_size((*refbox).data_size);
            ALLOC.dealloc(refbox as *mut u8, layout);
        }
    }
}

#[no_mangle]
pub unsafe fn get_data(refbox: *mut RefBox) -> *mut u8 {
    (refbox as *mut u8).add(mem::size_of::<RefBox>())
}
