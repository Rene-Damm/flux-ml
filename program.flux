// Most of the stuff here would "normally" be in the "standard library".

abstract type Object;

immutable builtin type Integer;
immutable builtin type Float;
immutable builtin type Character;

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

//Call "Length"?
abstract field Count( FiniteCollection ) : Integer;

builtin method Print( String );
builtin method Random< Integer >() : Integer;
builtin method Random< Float >() : Float;

builtin method \+( Left : String, Right : String ) : String;
builtin method \+( mutable MutableString, String ) : mutable MutableString;

builtin method Default< Object >() : Object;

method Hello() : String
{
    return "Hello";
}

method Subject() : String
{
	local N := Random< Integer > % 5;
	switch ( N )
	{
		case 0: return " World!";
		case 1: return " Joe!";
		case 2: return " Afterlife!";
		case 3: return " and Goodbye!";
		case 4: return " Unknown!";
	}
}

method Main()
{
    Print( Hello() + Subject() );
}

