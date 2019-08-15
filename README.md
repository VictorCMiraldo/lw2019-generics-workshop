# The Ins and Outs of Generic Programming

Generic Programming is the technique of writing programs that works over a class of arbitrary data types. That is how automatic toJSON and fromJSON are implemented in Haskell, for instance. They work by induction on the datatype they are translating to or from JSON. The variety of ways of writing generic programs can be daunting, each coming with their own pros and cons. In this workshop I will outline the ins and outs of some libraries. We will look into the trade offs and examples of each approach, including some state-of-the-art techniques. We start the journey with GHC.Generics, go into Generics.SOP and finish at Generics.MRSOP. We will cover simple examples such as generic serialization well into more complicated ones such as generic unification. These will require us to fiddle with the representation of our types and perform some operations such as annotating a recursive datatype and adding holes to a datatype in a generic fashion, which are useful in practice.

We will look into three main flavors of generic programming over regular data types and we will talk about more advanced concepts such as GADTs and Mutually Recursive families if time allows.
The topics I would like to cover are:

* Generic Equality
* Generic Serialization
* Generic Merkle Trees (which involve annotated fixpoints)
* Generic Unification (which involve adding holes to the types in question)

