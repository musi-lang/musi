use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::errors::{MusiError, MusiResult};

pub struct MemoryManager {
    alloc_counter: AtomicUsize,
    dealloc_counter: AtomicUsize,
    objects: HashMap<usize, HeapObject>,
    pinned: HashMap<usize, bool>,
}

#[derive(Debug)]
pub struct HeapObject {
    data: Vec<u8>,
    ref_count: AtomicUsize,
}

impl Clone for HeapObject {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            ref_count: AtomicUsize::new(self.ref_count.load(Ordering::SeqCst)),
        }
    }
}

impl HeapObject {
    fn new(data: Vec<u8>) -> Self {
        Self {
            data,
            ref_count: AtomicUsize::new(1),
        }
    }

    pub fn data(&self) -> &[u8] {
        &self.data
    }

    fn inc_ref(&self) {
        self.ref_count.fetch_add(1, Ordering::SeqCst);
    }

    fn dec_ref(&self) -> usize {
        self.ref_count
            .fetch_sub(1, Ordering::SeqCst)
            .saturating_sub(1)
    }

    pub fn ref_count(&self) -> usize {
        self.ref_count.load(Ordering::SeqCst)
    }
}

impl MemoryManager {
    pub fn new() -> Self {
        Self {
            alloc_counter: AtomicUsize::new(0),
            dealloc_counter: AtomicUsize::new(0),
            objects: HashMap::new(),
            pinned: HashMap::new(),
        }
    }

    fn get_or_err(&self, id: usize) -> MusiResult<&HeapObject> {
        self.objects
            .get(&id)
            .ok_or_else(|| MusiError::AllocationFailed {
                reason: format!("object {id} not found"),
            })
    }

    fn get_mut_or_err(&mut self, id: usize) -> MusiResult<&mut HeapObject> {
        self.objects
            .get_mut(&id)
            .ok_or_else(|| MusiError::AllocationFailed {
                reason: format!("object {id} not found"),
            })
    }

    pub fn alloc(&mut self, data: Vec<u8>) -> MusiResult<usize> {
        let id = self.alloc_counter.fetch_add(1, Ordering::SeqCst);
        self.objects.insert(id, HeapObject::new(data));
        Ok(id)
    }

    pub fn dealloc(&mut self, id: usize) -> MusiResult<()> {
        let obj = self.get_or_err(id)?;
        if obj.ref_count() > 1 {
            return Err(MusiError::RefCountError(
                "object still referenced".to_string(),
            ));
        }
        if self.pinned.contains_key(&id) {
            return Err(MusiError::RefCountError("object is pinned".to_string()));
        }
        self.objects.remove(&id);
        self.dealloc_counter.fetch_add(1, Ordering::SeqCst);
        Ok(())
    }

    pub fn get_object(&self, id: usize) -> Option<&HeapObject> {
        self.objects.get(&id)
    }

    pub fn inc_ref(&self, id: usize) -> MusiResult<()> {
        self.get_or_err(id)?.inc_ref();
        Ok(())
    }

    pub fn dec_ref(&mut self, id: usize) -> MusiResult<()> {
        let new_count = self.get_or_err(id)?.dec_ref();
        if new_count == 0 && !self.pinned.contains_key(&id) {
            self.objects.remove(&id);
            self.dealloc_counter.fetch_add(1, Ordering::SeqCst);
        }
        Ok(())
    }

    pub fn pin(&mut self, id: usize) -> MusiResult<()> {
        self.get_or_err(id)?;
        self.pinned.insert(id, true);
        Ok(())
    }

    pub fn unpin(&mut self, id: usize) -> MusiResult<()> {
        self.get_or_err(id)?;
        if !self.pinned.remove(&id).is_some() {
            return Err(MusiError::RefCountError("object not pinned".to_string()));
        }
        Ok(())
    }

    pub fn stats(&self) -> (usize, usize, usize, usize) {
        (
            self.alloc_counter.load(Ordering::SeqCst),
            self.dealloc_counter.load(Ordering::SeqCst),
            self.objects.len(),
            self.pinned.len(),
        )
    }
}

impl Default for MemoryManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alloc_dealloc() {
        let mut memmgr = MemoryManager::new();
        let data = vec![1, 2, 3, 4];
        let id = memmgr.alloc(data.clone()).unwrap();

        let obj = memmgr.get_object(id).unwrap();
        assert_eq!(obj.data(), &data);

        memmgr.dealloc(id).unwrap();
        assert!(memmgr.get_object(id).is_none());
    }

    #[test]
    fn test_ref_counting() {
        let mut memmgr = MemoryManager::new();
        let data = vec![1, 2, 3, 4];
        let id = memmgr.alloc(data).unwrap();

        memmgr.inc_ref(id).unwrap();
        let obj = memmgr.get_object(id).unwrap();
        assert_eq!(obj.ref_count(), 2);

        memmgr.dec_ref(id).unwrap();
        assert!(memmgr.get_object(id).is_some());

        memmgr.dec_ref(id).unwrap();
        assert!(memmgr.get_object(id).is_none());
    }

    #[test]
    fn test_pinning() {
        let mut memmgr = MemoryManager::new();
        let data = vec![1, 2, 3, 4];
        let id = memmgr.alloc(data).unwrap();

        memmgr.pin(id).unwrap();
        assert!(memmgr.pinned.contains_key(&id));

        memmgr.unpin(id).unwrap();
        assert!(!memmgr.pinned.contains_key(&id));
    }
}
