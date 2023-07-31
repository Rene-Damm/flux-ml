# A Compiler for a Simplified Version of Flux

This compiler is an exercise for myself to learn more about compiler construction (especially register allocation and SSA). It largely follows "Modern Compiler Implementation in ML" by Andrew W. Appel (by now a rather old textbook). The target architecture is X64.

It is implemented in [Standard ML](https://www.smlnj.org/).

Since the primary goal is __NOT__ to implement a compiler for Flux but to learn about compiler construction, this does not use LLVM (which would make implementing a full compiler just that much easier).

## How to Run It

Requires SML of New Jersey.

```
C:\sml
Standard ML of New Jersey (32-bit) v110.99.3 [built: Thu Jul 28 00:45:02 2022]
- CM.make("sources.cm");
- Main.run();
```

## The Language

Flux is my own language design from a good while back. It is an imperative, object-oriented language with multiple-dispatch, symmetric functions (all functions have one argument and one result; essentially like in functional languages like ML), a powerful type system, and &ndash; in the "real" thing &ndash; a module system, too. It uses a C-style syntax and roughly looks like this.

```
// Types have no inherent structure. You just declare them.
type MyType;
type MyOtherType;

type MyDerivedType : MyType;
type MyOtherDerivedType : MyType & MyOtherType; // Union.

// Singleton types are declared with `object`.
object MySingleton;

// You can alias types. Any type.
type TypeAlias = MyOtherDerivedType;

// This allows for some pretty powerful stuff. Like these two types from the "standard library".
object True;
object False;
type Boolean = True | False;

// Fields can be attached to types at any time.
field Name( MyType ) : String;

// Any types.
field Value( MyDerivedType | MyOtherDerivedType ) : Float;

// Methods are also freely defined, only that they aren't really attached to any single
// type. All methods of the same name form a function and the most appropriate
// method for each function call is selected automatically.
method DoSomething( Argument : MyType )
{
	Print( "Foo" );
}

// Type annotations are done with ": Type". However, if a name and a type is expected
// somewhere and there's only a type, the name is inferred.
// Here, the name will simply be `MyType` (as you'd expect).
method AlsoDoSomething( MyType )
{
}

// Mutability is explicit. Every type that isn't explicitly declared immutable
// has both a mutable and immutable variant.
method DoSomethingElse( mutable MyType )
{
	MyType.Name = "Charles";
}

// Loops are expressed using [Sather](https://www.gnu.org/software/sather/)-style iterators.
method SomethingWithALoop()
{
	// There's no `for` or `while` or other iteration statement.
	// There's just loops that are driven by iterators. Arbitrarily many of them.
	loop
	{
		// := is assignment with type inference.
		local I := 0.UpTo!( 10 ); // Like in Sather, iterator methods are suffixed with `!`.

		Print( I.To< String > );
	}
}

// You can define your own iterators.
iterator method EverySecondElement!< Object >( List< Object > ) : Object
{
	loop
	{
		local I := 0.Up!; // Just infinitely counts up from 0.
		local Value := List.Elements!; // Yields every element in List.

		if ( I % 2 == 0 )
			yield Value;
	}
}

// Types and methods can be fully parameterized. Unlike in many other languages, however,
// the parameterized type is itself a full type (imagine the `List<T>` class in C# being
// usable as just `List` and simply denoting a list of unspecified value type; its `Count`
// property is fully usable without ever knowing the value type).
// Note the same rule applies here about inferring names when no explicit names are given.
// Note that `Object` is the root type like in C# and many other object-oriented languages.
type MyParameterizedType< Object >; // Same as MyParameterizedType< Object : Object >
method MyParameterizedMethod< Object >( Object )
{
}

// Just like for value arguments, the most appropriate implementation is selected based on
// the actual type arguments supplied to a call (only that this happens at compile time, unlike
// with value arguments).
method Random< Integer >() : Integer
{
	return 123;
}
method Random< Float >() : Float
{
	return 123.0;
}

// Type aliases may have parameters, too. Like this one from the "standard library".
object Nothing;
type Optional< Object > = Object | Nothing;

// Methods can be "composed" using `before`, `after`, and `around` methods. Does what
// you'd expect it to.
method SomeMethod()
{
	Print( "bar" );
}
before method SomeMethod()
{
	Print( "foo" );
}

// Calling SomeMethod() prints `foobar`.

// Finally, you can attach pre- and post-conditions to methods.
method MethodWithConditions( Integer, mutable List< Integer > )
	pre Integer >= 12
	post List.Contains( Integer )
{
}

// And define invariants.
invariant method ListCanNeverContain12( List< Integer > ) : Boolean
{
	return !List.Contains( 12 );
}
```

## Differences/Deviations to/from "The Real Thing"

- Uses a lexer and thus tokens. Means there's reserved words and no context dependence.
- Is case-*sensitive*. The "real" thing isn't.
- Can handle only a restricted subset of type *argument* expressions. Type expressions cannot be applied to arbitrary values; can only be applied to identifiers.
  * This restriction is to allow the syntax to be parsed with LALR(1).
- Does not support modules.
- In fact, does not support more than a single compilation unit.

