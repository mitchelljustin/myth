#![allow(clippy::not_unsafe_ptr_arg_deref)]
use std::alloc::{GlobalAlloc, Layout};

static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[repr(C)]
pub struct RefBox {
    strong: usize,
    data_size: usize,
}

impl RefBox {
    fn layout_for_data_size(data_size: usize) -> Layout {
        let size = size_of::<RefBox>() + data_size;
        let align = align_of::<usize>();
        unsafe { Layout::from_size_align_unchecked(size, align) }
    }

    fn layout(&self) -> Layout {
        Self::layout_for_data_size(self.data_size)
    }

    fn data_ptr(ref_box: *mut RefBox) -> *mut u8 {
        unsafe { ref_box.add(1) as *mut u8 }
    }
}

#[no_mangle]
pub fn new(data_size: usize) -> *mut RefBox {
    unsafe {
        let layout = RefBox::layout_for_data_size(data_size);
        ALLOC.alloc(layout) as *mut RefBox
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
            let layout = (*refbox).layout();
            ALLOC.dealloc(refbox as *mut u8, layout);
        }
    }
}

#[no_mangle]
pub fn get_data(refbox: *mut RefBox) -> *mut u8 {
    unsafe { (refbox as *mut u8).add(size_of::<RefBox>()) }
}

#[no_mangle]
pub unsafe fn i32_load(refbox: *mut RefBox, offset: usize) -> i32 {
    let data_ptr = RefBox::data_ptr(refbox).add(offset) as *mut i32;
    *data_ptr
}

#[no_mangle]
pub unsafe fn i32_store(refbox: *mut RefBox, value: i32, offset: usize) {
    let data_ptr = RefBox::data_ptr(refbox).add(offset) as *mut i32;
    *data_ptr = value;
}
