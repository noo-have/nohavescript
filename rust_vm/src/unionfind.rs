use std::collections::HashMap;
use std::fmt::Debug;

use crate::analyzer::Type;

/// UnionFind data structure
/// It acts by holding an array of pointers to parents, together with the size of each subset
#[derive(Debug, Default)]
pub struct UnionFind {
    payloads: HashMap<Type, usize>, // we are going to manipulate indices to parent, thus `usize`. We need a map to associate a value to its index in the parent links array
    parent_links: Vec<usize>, // holds the relationship between an item and its parent. The root of a set is denoted by parent_links[i] == i
    sizes: Vec<usize>,        // holds the size
    count: usize,
    pub representing_meta: HashMap<usize, Type>,
}

impl UnionFind {
    /// Inserts a new item (disjoint) in the data structure
    pub fn insert(&mut self, item: Type) {
        let key = self.payloads.len();
        self.parent_links.push(key);
        self.sizes.push(1);
        self.payloads.insert(item.clone(), key);
        self.count += 1;
        self.representing_meta.insert(key, item);
    }

    pub fn id(&self, value: &Type) -> Option<usize> {
        self.payloads.get(value).copied()
    }
    /// 判断该元素是否存在UF里
    pub fn has(&self, value: &Type) -> bool {
        matches!(self.payloads.get(value), Some(_))
    }
    /// Returns the key of an item stored in the data structure or None if it doesn't exist
    pub fn find(&self, value: &Type) -> Option<usize> {
        self.id(value).map(|id| self.find_by_key(id))
    }
    /// 链接两个 type
    ///
    /// 前者总是会被合并到后者
    ///
    /// 因此,后者的代表元不会变化,而前者的代表元会被设置成后者的代表元
    pub fn union(&mut self, t1: &Type, t2: &Type) -> Option<bool> {
        match (self.find(t1), self.find(t2)) {
            (Some(k1), Some(k2)) => Some(self.union_by_key(k1, k2)),
            _ => None,
        }
    }

    /// Returns the parent of the element given its id
    fn find_by_key(&self, key: usize) -> usize {
        let mut id = key;
        while id != self.parent_links[id] {
            id = self.parent_links[id];
        }
        id
    }
    /// 寻找代表元
    pub fn find_representing_meta(&self, key: usize) -> Option<Type> {
        self.representing_meta.get(&key).cloned()
    }
    /// Unions the sets containing id1 and id2
    fn union_by_key(&mut self, key1: usize, key2: usize) -> bool {
        let root1 = self.find_by_key(key1);
        let root2 = self.find_by_key(key2);
        if root1 == root2 {
            return false; // they belong to the same set already, no-op
        }

        // Attach the smaller set to the larger one
        // 优化深度
        // if self.sizes[root1] < self.sizes[root2] {
        //     self.parent_links[root1] = root2;
        //     self.sizes[root2] += self.sizes[root1];
        // } else {
        //     self.parent_links[root2] = root1;
        //     self.sizes[root1] += self.sizes[root2];
        // }
        self.representing_meta
            .insert(root2, self.find_representing_meta(root1).unwrap());
        self.parent_links[root2] = root1;
        self.sizes[root1] += self.sizes[root2];
        self.count -= 1; // we had 2 disjoint sets, now merged as one
        true
    }

    /// Checks if two items belong to the same set
    ///
    /// #_Examples:
    ///
    /// ```
    /// use the_algorithms_rust::data_structures::UnionFind;
    /// let mut uf = UnionFind::from_iter(["A", "B"]);
    /// assert!(!uf.is_same_set(&"A", &"B"));
    ///
    /// uf.union(&"A", &"B");
    /// assert!(uf.is_same_set(&"A", &"B"));
    ///
    /// assert!(!uf.is_same_set(&"A", &"C"));
    /// ```
    pub fn is_same_set(&self, item1: &Type, item2: &Type) -> bool {
        matches!((self.find(item1), self.find(item2)), (Some(root1), Some(root2)) if root1 == root2)
    }

    // / Returns the number of disjoint sets
    // /
    // / # Examples
    // /
    // / ```
    // / use the_algorithms_rust::data_structures::UnionFind;
    // / let mut uf = UnionFind::with_capacity(5);
    // / assert_eq!(0, uf.count());
    // /
    // / uf.insert("A");
    // / assert_eq!(1, uf.count());
    // /
    // / uf.insert("B");
    // / assert_eq!(2, uf.count());
    // /
    // / uf.union(&"A", &"B");
    // / assert_eq!(1, uf.count())
    // / ```
    // pub fn count(&self) -> usize {
    //     self.count
    // }
}

impl UnionFind {
    /// Creates a new UnionFind data structure from an iterable of disjoint elements
    pub fn from_iter<I: IntoIterator<Item = Type>>(iter: I) -> Self {
        let mut uf = UnionFind::default();
        for i in iter {
            uf.insert(i);
        }
        uf
    }
}
