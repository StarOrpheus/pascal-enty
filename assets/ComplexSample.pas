{I use (very) modified source from http://sandbox.mc.edu/~bennet/cs404/doc/sort_pas.html}

PROGRAM Sort;

VAR
    i, j, tmp, size, kek : integer;
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
    kek := tmp div 5;
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

    for i := 2 to size do
        assert(arr[i-1] <= arr[i]);

    FOR i := 1 TO size DO
        Write(arr[i], ' ');
    writeln;

    Writeln('Wrong sum = ', wrongSum(42, 16));
    assert(wrongSum(42, 16) = kek + 42 + 16);

    { Check asserts with assert(false); }
END.