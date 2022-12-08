//! A pure Rust, no-std implementation of [bit fields](https://en.wikipedia.org/wiki/Bit_field)
#![forbid(clippy::exit, clippy::expect_used, clippy::panic, clippy::unwrap_used)]
#![deny(clippy::unimplemented, clippy::unreachable)]
#![warn(clippy::dbg_macro, clippy::todo, clippy::try_err)]
#![warn(clippy::cargo, clippy::nursery, clippy::pedantic, missing_docs)]
#![no_std]

use core::marker::PhantomData;

/// Allows a value to be represented as a bit flag
pub trait Flag {
    /// Returns the value's bit flag offset
    ///
    /// This is used as a shift offset, so it should not be greater than or equal to the total number of bits within the field
    fn get_offset(&self) -> u8;
}

/// Allows a value to be used as an immutable bit field
pub trait ConstField<T: Flag>: Sized {
    /// The type stored within the bit field, or at least a type that can be converted to and from it
    #[cfg(feature = "serde")]
    type Inner: for<'de> serde::Deserialize<'de>;
    /// The type stored within the bit field, or at least a type that can be converted to and from it
    #[cfg(not(feature = "serde"))]
    type Inner;

    /// Creates a new empty bit field
    fn new() -> Self;
    /// Creates a new bit field containing the given value
    fn new_with(value: Self::Inner) -> Self;

    /// Returns the bit field's inner value
    fn get_inner(&self) -> Self::Inner;

    /// Returns `true` if the provided bit flag is stored within the bit field
    fn contains(&self, flag: impl Into<T>) -> bool;
    /// Returns `true` if all of the provided bit flags are stored within the bit field
    fn contains_all(&self, flags: &[impl Into<T> + Clone]) -> bool {
        flags.iter().all(|flag| self.contains(flag.clone()))
    }
    /// Returns `true` if any of the provided bit flags are stored within the bit field
    fn contains_any(&self, flags: &[impl Into<T> + Clone]) -> bool {
        flags.iter().any(|flag| self.contains(flag.clone()))
    }
}

/// Allows a value to be used as a mutable bit field
pub trait Field<T: Flag>: ConstField<T> {
    /// Adds the given bit flag into the bit field
    #[must_use]
    fn insert(self, flag: impl Into<T>) -> Self;
    /// Adds all of the given bit flags into the bit field
    #[must_use]
    fn insert_all(mut self, flags: &[impl Into<T> + Clone]) -> Self {
        for flag in flags {
            self = self.insert(flag.clone());
        }

        self
    }

    /// Removes the given bit flag from the bit field
    #[must_use]
    fn remove(self, flag: impl Into<T>) -> Self;
    /// Removes all of the given bit flags into the bit field
    #[must_use]
    fn remove_all(mut self, flags: &[impl Into<T> + Clone]) -> Self {
        for flag in flags {
            self = self.remove(flag.clone());
        }

        self
    }

    /// Inverts the flags within the bit field
    #[must_use]
    fn not(self) -> Self;
}

/// Generates an enum of the given flags and their offsets
#[macro_export]
macro_rules! flags {
    ($visibility:vis $name:ident {$($flag:ident = $offset:literal,)*}) => {
        #[repr(u8)]
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[allow(missing_docs)]
        $visibility enum $name {
            $($flag = $offset,)*
        }

        impl Flag for $name {
            fn get_offset(&self) -> u8 {
                *self as u8
            }
        }
    };
}

macro_rules! bitfield {
    (const $id:ident($n:ty); $doc:literal$(, $mark:literal)?) => {
        #[doc = $doc]
        $(#[doc = $mark])?
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub struct $id<T: Flag>($n, PhantomData<T>);

        impl<T: Flag> ConstField<T> for $id<T> {
            type Inner = $n;

            fn new() -> Self {
                Self::new_with(0)
            }
            fn new_with(value: Self::Inner) -> Self {
                Self(value, PhantomData)
            }
            fn get_inner(&self) -> Self::Inner {
                self.0
            }
            fn contains(&self, flag: impl Into<T>) -> bool {
                self.0 & (1 << flag.into().get_offset()) != 0
            }
        }

        impl<T: Flag> Default for $id<T> {
            fn default() -> Self {
                Self::new()
            }
        }

        #[cfg(feature = "serde")]
        impl<T: Flag> serde::Serialize for $id<T> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                self.0.serialize(serializer)
            }
        }
        #[cfg(feature = "serde")]
        impl<'de, T: Flag> serde::Deserialize<'de> for $id<T> {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                let inner = <Self as ConstField<T>>::Inner::deserialize(deserializer)?;

                Ok(Self::new_with(inner))
            }
        }
    };
    ($id:ident($n:ty); $doc:literal$(, $mark:literal)?) => {
        bitfield!(const $id($n); $doc$(, $mark)?);

        impl<T: Flag> Field<T> for $id<T> {
            fn insert(mut self, flag: impl Into<T>) -> Self {
                self.0 |= 1 << flag.into().get_offset();
                self
            }
            fn remove(mut self, flag: impl Into<T>) -> Self {
                self.0 &= !(1 << flag.into().get_offset());
                self
            }
            fn not(mut self) -> Self {
                self.0 = !self.0;
                self
            }
        }
    };
    ($n:ty => const $imm:ident, $mut:ident; $doc:literal) => {
        bitfield!(const $imm($n); $doc, "\nThis is the immutable form and can be converted with `From::from` or `Into::into`.");
        bitfield!($mut($n); $doc, "\nThis is the mutable form and can be converted with `From::from` or `Into::into`.");

        impl<T: Flag> From<$imm<T>> for $mut<T> {
            fn from(value: $imm<T>) -> Self {
                Self::new_with(value.get_inner())
            }
        }
        impl<T: Flag> From<$mut<T>> for $imm<T> {
            fn from(value: $mut<T>) -> Self {
                Self::new_with(value.get_inner())
            }
        }
    };
}

bitfield!(u8 => const ConstBitField8, BitField8; "A bit field containing a `u8`");
bitfield!(u16 => const ConstBitField16, BitField16; "A bit field containing a `u16`");
bitfield!(u32 => const ConstBitField32, BitField32; "A bit field containing a `u32`");
bitfield!(u64 => const ConstBitField64, BitField64; "A bit field containing a `u64`");
bitfield!(u128 => const ConstBitField128, BitField128; "A bit field containing a `u128`");

#[cfg(test)]
mod tests {
    use super::*;

    flags!(Byte {
        Zero = 0,
        One = 1,
        Two = 2,
        Three = 3,
        Four = 4,
        Five = 5,
        Six = 6,
        Seven = 7,
    });

    #[test]
    fn new_empty() {
        let field = BitField8::<Byte>::new();

        assert_eq!(0, field.get_inner());
    }
    #[test]
    fn new_filled() {
        let field = BitField8::<Byte>::new_with(0b0010_0110);

        assert_ne!(0, field.get_inner());
    }
    #[test]
    fn contains() {
        let field = BitField8::<Byte>::new_with(0b0010_0110);

        assert!(field.contains_all(&[Byte::One, Byte::Two, Byte::Five]));
        assert!(!field.contains_any(&[
            Byte::Zero,
            Byte::Three,
            Byte::Four,
            Byte::Six,
            Byte::Seven,
        ]));
    }
    #[test]
    fn insert() {
        let mut field = BitField8::<Byte>::new_with(0b0000_0111);

        assert!(field.contains_all(&[Byte::Zero, Byte::One, Byte::Two]));

        field = field.insert_all(&[Byte::Five, Byte::Seven]);

        assert!(!field.contains_any(&[Byte::Three, Byte::Four, Byte::Six]));
        assert!(field.contains_all(&[Byte::Zero, Byte::One, Byte::Two, Byte::Five, Byte::Seven]));
    }
    #[test]
    fn remove() {
        let mut field = BitField8::<Byte>::new_with(0b0000_0111);

        assert!(field.contains_all(&[Byte::Zero, Byte::One, Byte::Two]));

        field = field.remove_all(&[Byte::Zero, Byte::Two]);

        assert!(field.contains(Byte::One));
    }
    #[test]
    fn not() {
        let mut field = BitField8::<Byte>::new_with(0b0000_1111);

        assert!(field.contains_all(&[Byte::Zero, Byte::One, Byte::Two, Byte::Three]));

        field = field.not();

        assert!(field.contains_all(&[Byte::Four, Byte::Five, Byte::Six, Byte::Seven]));
    }
}
