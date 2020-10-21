{I use (very) modified source from http://sandbox.mc.edu/~bennet/cs404/doc/sort_pas.html}

PROGRAM Sort;
const kek = 5;

VAR
    i, j, tmp, size: integer;
    arr: ARRAY [1..50] OF Integer;

{ Procedural + IO sample}
PROCEDURE ReadArr;
    var tmp : integer;
BEGIN
    tmp := 42;
	readln(size);
    writeln('Size == ', size);
    for i := 1 to size do
    BEGIN
        Readln(arr[i]);
    end;
END;

{ Functional sample }
function wrongSum(a, b : integer): integer;
BEGIN
    wrongSum := kek + a + b;
END;

BEGIN
    tmp := 43;
    ReadArr;
    writeln('Size after procedure call = ', size);

    { State rollback example, global variable not affected by function call }
    assert(tmp = 43);

    FOR i := size - 1 DOWNTO 1 DO
        FOR j := 1 TO i DO
            IF arr[j] > arr[j + 1] THEN BEGIN
                tmp := arr[j];
                arr[j] := arr[j + 1];
                arr[j + 1] := tmp;
            END;

    FOR i := 1 TO size DO
        Write(arr[i], ' ');
    writeln;

    Writeln('Wrong sum = ', wrongSum(42, 16));
END.