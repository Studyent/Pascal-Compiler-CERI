VAR		a,b,c,i,j,n :	INTEGER;
		c1,c2:	CHAR;
		num, denum, frac:		DOUBLE.
c1:='f';
c2:='a';
num:=1.0;
denum:=1.0;
frac:=num/denum;
a:=10;
IF a<10 THEN
    a:=1
ELSE
    a:=2;
FOR a:=2 TO 10 DO
    a:=a+1;

FOR b:=4 DOWNTO 2 DO
    b:=b-1;

FOR i:=1 TO 3 DO
    FOR j:=3 DOWNTO 1 DO
        b:=b+1;


n:=2;
 
CASE n OF
    1: n:=n-1;
    2: n:=n+1;
    3,4: n:=n+2-1
END;

REPEAT
    b:=b+1;
    REPEAT
        a:=a+1;
    UNTIL a > 5;
UNTIL b >= 5;

.
