--
-- The author of this program disclaims copyright.
--

with Calc_Ada;

procedure Main is
   use Calc_Ada;
   pParser : Void_Access := ParseAlloc (malloc);
begin
    -- First input:
    -- 15 / 5
    Parse (pParser, INTEGER, 15);
    Parse (pParser, DIVIDE, 0);
    Parse (pParser, INTEGER, 5);
    Parse (pParser, 0, 0);

    -- Second input:
    -- 50 + 125
    Parse (pParser, INTEGER, 50);
    Parse (pParser, PLUS, 0);
    Parse (pParser, INTEGER, 125);
    Parse (pParser, 0, 0);

    -- Third input:
    -- 50 * 125 + 125
    Parse (pParser, INTEGER, 50);
    Parse (pParser, TIMES, 0);
    Parse (pParser, INTEGER, 125);
    Parse (pParser, PLUS, 0);
    Parse (pParser, INTEGER, 125);
    Parse (pParser, 0, 0);

    ParseFree (pParser, free);
end Main;
