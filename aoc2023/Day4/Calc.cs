using LanguageExt.ClassInstances;
using LanguageExt.UnsafeValueAccess;

namespace aoc2023.Day4;
using LanguageExt;
using Parser = LanguageExt.Parsec;
using static LanguageExt.Prelude;
using LanguageExt.Parsec;
using LanguageExt.Parsec;
using static LanguageExt.Prelude;
using static LanguageExt.Parsec.Prim;
using static LanguageExt.Parsec.Char;
using static LanguageExt.Parsec.PString;
using static LanguageExt.Parsec.Expr;
using static LanguageExt.Parsec.Token;
using static LanguageExt.Parsec.Indent;
using static LanguageExt.TypeClass;


public record Line(int Number, IEnumerable<int> Winning, IEnumerable<int> Actual);

public record Puzzle(IEnumerable<Line> Lines);


public class Calc
{
    public static Either<string, IEnumerable<Line>> Parse(IEnumerable<string> ss)
    {
        // var number = asInteger(many1(digit));
        // var line = from const
        // var p = Parser.Char.spaces;
        ss = ss.Select(s => s.Replace(" |  ", "|"));
        ss = ss.Select(s => s.Replace(" | ", "|"));
        var lineParser = from _ in str("Card")
            from spaces in many1(space)
            from number in asInteger(many1(digit))
            from sep in ch(':')
            from spaces2 in many1(space)
            from winnings in sepBy1(asInteger(many1(digit)), many(ch(' ')))
            from sep2 in ch('|')
            from results in sepBy1(asInteger(many1(digit)), many(ch(' ')))
            select new Line(number.Value(), winnings.Map(w => w.Value()), results.Map(r => r.Value()));
        var pz = from s in ss
            select parse(lineParser ,s).ToEither();
        return pz.Traverse(x => x);
    }

    public static long Calc1(IEnumerable<Line> lines)
    {
        var calcLine = (Line line) =>
        {
            var winCnt = line.Actual.Count(x => line.Winning.Any(w => w == x));
            return winCnt == 0 ? 0 : Convert.ToInt32(Math.Pow(2, winCnt-1));
        };
        return lines.Select(calcLine).Sum();
    }
    public static long Calc2(IEnumerable<Line> lines)
    {
        var calcLine = (Line line) =>
        {
            var winCnt = line.Actual.Count(x => line.Winning.Any(w => w == x));
            return winCnt;
        };

        var res = lines.Select(calcLine);
        List<long> ress = new();
        int i = 0;
        foreach (var lookupCnt in res.Reverse())
        {
            var lookupBound = Math.Min(lookupCnt, i);
            ress.Add(1 + ress.Slice(i-lookupBound, lookupBound).Sum());
            i++;
        }

        return ress.Sum();
    }
}