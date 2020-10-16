{source: http://sandbox.mc.edu/~bennet/cs404/doc/sort_pas.html}

PROGRAM Sort;
const kek = 5;

VAR
    i, j, tmp, size: integer;
    arr: ARRAY [1..50] OF Integer;

PROCEDURE ReadArr(VAR size: Integer);
BEGIN
	readln(size);
    for i := 1 to size do
        read(arr[i]);
END;

BEGIN
    ReadArr(size);

    FOR i := size - 1 DOWNTO 1 DO
        FOR j := 1 TO i DO 
            IF arr[j] > arr[j + 1] THEN BEGIN
                tmp := arr[j];
                arr[j] := arr[j + 1];
                arr[j + 1] := tmp;
            END;

	writeln('Size: ', size);
    FOR i := 1 TO size DO
        write(arr[i], ' ')
END.