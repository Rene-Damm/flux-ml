
abstract type Object;
immutable type Integer;
immutable type Float;

object True;
object False;

object Nothing;

type Boolean = True | False;
type Optional<Object> = Object | Nothing;

method First( Float ) : Integer
{
	return 123;
}

