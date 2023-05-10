
abstract type Object;
builtin immutable type Integer;
builtin immutable type Float;

object True;
object False;

type Boolean = True | False;

method First( Float ) : Integer
{
	return 123;
}

