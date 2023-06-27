// Most of the stuff here would "usually" be in the "standard library".

abstract type Object;

immutable type Integer;
immutable type Float;
immutable type Character;

object True;
object False;
type Boolean = True | False;

object Nothing;
type Optional< Object > = Object | Nothing;

abstract type Collection< Object >;
	abstract type FiniteCollection< Object >;
	abstract type Sequence< Object > : Collection< Object >;

abstract type String : Sequence< Character > & FiniteCollection< Character >;
	abstract type ImmutableString : String;
	abstract type MutableString : String;

abstract field Count( FiniteCollection ) : Integer;

method Hello() : String
{
	return "Hello";
}

method First( Float ) : Integer
{
	return 123;
}

