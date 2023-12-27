using LanguageExt.ClassInstances;
using LanguageExt.UnsafeValueAccess;

namespace aoc2023.Day5;
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


public record RangeMap(long ToRangeStart, long FromRangeStart, long Len);


public class Calc
{
    public static Either<string, (List<long>, List<List<RangeMap>>)> Parse(string ss)
    {
        ss = ss.Replace("\n\n", "|");
        var longParser = many1(digit).Map(x => new string(x.ToArray<char>())).Map(long.Parse);
        var seedsParser = from _ in str("seeds: ")
            from seeds in sepBy1(longParser, many(ch(' ')))
            select seeds;
        var mapParser = from toRangeStart in longParser
            from _ in spaces
            from fromRangeStart in longParser
            from _s in spaces
            from rangeLen in longParser
            select new RangeMap(toRangeStart, fromRangeStart, rangeLen);
        var mapsParser = from _ in many1(choice(letter, space, ch('-')))
            from sep in ch(':')
            from sep2 in ch('\n')
            from result in sepBy1(mapParser, ch('\n'))
            select result.ToList();
        var parser = from seeds in seedsParser
            from sep in ch('|')
            from maps in sepBy1(mapsParser, ch('|'))
            select (seeds.ToList(), maps.ToList());
        return parse(parser, ss).ToEither();
    }
    
    public static long Calc1( (List<long>, List<List<RangeMap>>) puzzle)
    {
        var (seeds, maps) = puzzle;
        var calcLocation = (long x) =>
        {
            long start = x;
            foreach (var map in maps)
            {
                foreach (var range in map)
                {
                    if (range.FromRangeStart <= start && range.FromRangeStart + range.Len >= start)
                    {
                        start = start - range.FromRangeStart + range.ToRangeStart;
                        break;
                    }
                }
            }

            return start;
        };
        return seeds.Select(calcLocation).Min();
    }

    public static List<(long, long)> CalcNext(List<RangeMap> mapping, (long, long) range)
    {
        var (first, length) = range;
        List<(long, long)> result = new();
        List<(long, long)> mapped = new();
        foreach (var mapRange in mapping)
        {
            if (first + length <= mapRange.FromRangeStart) continue;
            if (first >= mapRange.FromRangeStart + mapRange.Len) continue;
            var mapFirst = Math.Max(mapRange.FromRangeStart, first);
            var mapLast = Math.Min(mapRange.FromRangeStart+mapRange.Len, first+length);
            var mappedLen = mapLast - mapFirst;
            result.Add((mapFirst - mapRange.FromRangeStart + mapRange.ToRangeStart, mappedLen));
            mapped.Add((mapFirst, mappedLen));
        }

        (long, long)? prevChunk = null;
        var orderMapped = mapped.Order();
        if (mapped.FirstOrDefault((first+length,0)).Item1 > first)
        { 
            result.Add((first, mapped.FirstOrDefault((first+length,0)).Item1-first));
        }
        if (mapped.Count !=0 && mapped.Last().Item1+mapped.Last().Item2 < first+length)
        { 
            result.Add((mapped.Last().Item1+mapped.Last().Item2, first+length-mapped.Last().Item1-mapped.Last().Item2));
        }
        // Handle missing elements before or after mapping
        foreach (var chunk in mapped.Order())
        {
            if (prevChunk is null)
            {
                prevChunk = chunk;
            }
            else
            {
                var prevEnd = prevChunk.Value.Item1 + prevChunk.Value.Item2;
                if (prevEnd < chunk.Item1-1)
                {
                    result.Add((prevEnd, chunk.Item1-prevEnd));
                }
            }
        }
        return result;
    }

    public static (long, long) Calc2( (List<long>, List<List<RangeMap>>) puzzle)
    {
        var (seeds, maps) = puzzle;
        List<(long, long)> ranges = new();
        
        long? fst = null;
        foreach (var seed in seeds)
        {
            if (fst is null)
            {
                fst = seed;
            }
            else
            {
                ranges.Add((fst.Value, seed));
                fst = null;
            }
        }
        var calcLocation = ((long, long) rr) =>
        {
            List<(long, long)> ranges = new();
            ranges.Add((rr.Item1, rr.Item2));
            foreach (var map in maps)
            {
                
                List<(long, long)> newRanges = new();
                foreach (var range in ranges)
                {
                    var resRange = CalcNext(map, range);
                    newRanges.AddRange(resRange);
                }
                ranges = newRanges;
            }
            return ranges;
        };
        return ranges.SelectMany(calcLocation).Min();
    }
}