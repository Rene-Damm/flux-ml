
abstract type Object;
immutable type Integer;
immutable type Float;
abstract type String;

object True;
object False;

object Nothing;

type Boolean = True | False;
type Optional<Object> = Object | Nothing;

method First( Float ) : Integer
{
	return 123;
}

