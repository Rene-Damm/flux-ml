// Most of the stuff here would "normally" be in the "standard library".

abstract type Object;

immutable type Integer;
immutable type Float;
immutable type Character;

immutable builtin object True;
immutable builtin object False;
type Boolean = True | False;

immutable builtin object Nothing;
type Optional< Object > = Object | Nothing;

abstract type Collection< Object >;
    abstract type FiniteCollection< Object >;
    abstract type Sequence< Object > : Collection< Object >;

abstract type String : Sequence< Character > & FiniteCollection< Character >;
    abstract immutable type ImmutableString : String;
    abstract type MutableString : String;

abstract field Count( FiniteCollection ) : Integer;

builtin method Print( String );

builtin method \+( Left : String, Right : String ) : String;
builtin method \+( mutable MutableString, String ) : mutable MutableString;

method Hello() : String
{
    return "Hello";
}

method Main()
{
    Print( Hello() + " World!" );
}

