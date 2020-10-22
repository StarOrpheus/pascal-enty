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
    assert(size <= 50);
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

    assert(kek < tmp,
           tmp > kek,
           kek <= tmp,
           tmp >= kek,
           tmp <> kek);
    tmp := kek;
    assert(not (kek < tmp),
           not (tmp > kek),
           not (kek > tmp),
           not (tmp < kek),
           tmp = kek,
           not (tmp <> kek));

    tmp := 43;
    assert(tmp = 43,
           tmp * 2 = 86,
           tmp div 3 = 14,
           tmp mod 3 = 1);

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