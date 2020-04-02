pub trait InnerType {
    type Type;
}

pub trait Functor<T: InnerType> {
    fn fmap<F: Fn(<T as InnerType>::Type) -> <T as InnerType>::Type>(self, f: F) -> T;
}

impl<T> InnerType for Box<T> {
    type Type = T;
}

impl<T> Functor<Box<T>> for Box<T> {
    fn fmap<F: Fn(T) -> T>(self, f: F) -> Box<T> {
        Box::new(f(*self))
    }
}
