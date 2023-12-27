using System.Collections.Immutable;
using FluentAssertions;
using Xunit;
using static LanguageExt.Prelude;
using LanguageExt.Parsec;
using LanguageExt.Parsec;
using LanguageExt.SomeHelp;
using static LanguageExt.Prelude;
using static LanguageExt.Parsec.Prim;
using static LanguageExt.Parsec.Char;
using static LanguageExt.Parsec.PString;
using static LanguageExt.Parsec.Expr;
using static LanguageExt.Parsec.Token;
using static LanguageExt.Parsec.Indent;
using static LanguageExt.TypeClass;

namespace aoc2023.Day5;

public class Test
{
    [Fact]
    public void Parsing()
    {
        var data = """
                   seeds: 79 14 55 13
                   
                   seed-to-soil map:
                   50 98 2
                   52 50 48
                   
                   soil-to-fertilizer map:
                   0 15 37
                   37 52 2
                   39 0 15
                   
                   fertilizer-to-water map:
                   49 53 8
                   0 11 42
                   42 0 7
                   57 7 4
                   
                   water-to-light map:
                   88 18 7
                   18 25 70
                   
                   light-to-temperature map:
                   45 77 23
                   81 45 19
                   68 64 13
                   
                   temperature-to-humidity map:
                   0 69 1
                   1 0 69
                   
                   humidity-to-location map:
                   60 56 37
                   56 93 4
                   """;
        var result = Calc.Parse(data);
        result.IsRight.Should().BeTrue();
    }
    
    [Fact]
    public void Sample1()
    {
        var data = """
                   seeds: 79 14 55 13

                   seed-to-soil map:
                   50 98 2
                   52 50 48

                   soil-to-fertilizer map:
                   0 15 37
                   37 52 2
                   39 0 15

                   fertilizer-to-water map:
                   49 53 8
                   0 11 42
                   42 0 7
                   57 7 4

                   water-to-light map:
                   88 18 7
                   18 25 70

                   light-to-temperature map:
                   45 77 23
                   81 45 19
                   68 64 13

                   temperature-to-humidity map:
                   0 69 1
                   1 0 69

                   humidity-to-location map:
                   60 56 37
                   56 93 4
                   """;
        var parsed = Calc.Parse(data);
        var result = parsed.Map(Calc.Calc1);
        result.Map(x => x.Should().Be(35));
    }
    
    [Fact(Skip = "For getting an answer")]
    public void Result1()
    {
        var parsed = Calc.Parse(File.ReadAllText("/Users/olegnykolyn/github/aoc/aoc2023/Day5/Data.txt"));
        var result = parsed.Map(Calc.Calc1);
        result.IsRight.Should().BeTrue();
        result.Map(x => x.Should().Be(13));
    }

    [Fact]
    public void Ranges1()
    {
        List<RangeMap> mapping = new List<RangeMap>()
        {
            new RangeMap(100, 10, 10)
        };
        var res = Calc.CalcNext(mapping, (1,2));
        res.Should().Equal(new (long, long)[]{(1,2)});
    }
    
    [Fact]
    public void Ranges2()
    {
        List<RangeMap> mapping = new List<RangeMap>()
        {
            new RangeMap(100, 10, 10)
        };
        var res = Calc.CalcNext(mapping, (8,2));
        res.Should().Equal(new (long, long)[]{(8,2)});
    }
    
    [Fact]
    public void Ranges3()
    {
        List<RangeMap> mapping = new List<RangeMap>()
        {
            new RangeMap(100, 10, 10)
        };
        var res = Calc.CalcNext(mapping, (8,4));
        res.Should().Equal(new (long, long)[]{(100, 2), (8, 2)});
    }
    
    [Fact]
    public void Ranges4()
    {
        List<RangeMap> mapping = new List<RangeMap>()
        {
            new RangeMap(100, 10, 10)
        };
        var res = Calc.CalcNext(mapping, (12,2));
        res.Should().Equal(new (long, long)[]{(102, 2)});
    }
    
    [Fact]
    public void Ranges5()
    {
        List<RangeMap> mapping = new List<RangeMap>()
        {
            new RangeMap(100, 10, 10)
        };
        var res = Calc.CalcNext(mapping, (18,4));
        res.Should().Equal(new (long, long)[]{(108, 2), (20, 2)});
    }
    
    [Fact]
    public void Ranges6()
    {
        List<RangeMap> mapping = new List<RangeMap>()
        {
            new RangeMap(100, 10, 10)
        };
        var res = Calc.CalcNext(mapping, (22,4));
        res.Should().Equal(new (long, long)[]{(22, 4)});
    }
    
    [Fact]
    public void Ranges7()
    {
        List<RangeMap> mapping = new List<RangeMap>()
        {
            new RangeMap(100, 10, 10)
        };
        var res = Calc.CalcNext(mapping, (18,2));
        res.Should().Equal(new (long, long)[]{(108, 2)});
    }
    
    [Fact]
    public void Ranges8()
    {
        List<RangeMap> mapping = new List<RangeMap>()
        {
            new RangeMap(100, 10, 10),
            new RangeMap(200, 20, 10),
        };
        var res = Calc.CalcNext(mapping, (18,4));
        res.Should().Equal(new (long, long)[]{(108, 2), (200, 2)});
    }
    
    [Fact]
    public void Ranges9()
    {
        List<RangeMap> mapping = new List<RangeMap>()
        {
            new RangeMap(100, 10, 10),
            new RangeMap(200, 22, 10),
        };
        var res = Calc.CalcNext(mapping, (18,6));
        res.Should().Equal(new (long, long)[]{(108, 2), (200, 2), (20, 2)});
    }
    
    [Fact]
    public void Ranges10()
    {
        List<RangeMap> mapping = new List<RangeMap>()
        {
            new RangeMap(100, 10, 10),
        };
        var res = Calc.CalcNext(mapping, (5,25));
        res.Should().Equal(new (long, long)[]{(100, 10), (5, 5), (20, 10)});
    }
    
    [Fact]
    public void Ranges11()
    {
        List<RangeMap> mapping = new List<RangeMap>()
        {
            new RangeMap(100, 10, 10)
        };
        var res = Calc.CalcNext(mapping, (20, 20));
        res.Should().Equal(new (long, long)[]{(20, 20)});
    }
    
    [Fact]
    public void Sample2()
    {
        var data = """
                   seeds: 79 14 55 13

                   seed-to-soil map:
                   50 98 2
                   52 50 48

                   soil-to-fertilizer map:
                   0 15 37
                   37 52 2
                   39 0 15

                   fertilizer-to-water map:
                   49 53 8
                   0 11 42
                   42 0 7
                   57 7 4

                   water-to-light map:
                   88 18 7
                   18 25 70

                   light-to-temperature map:
                   45 77 23
                   81 45 19
                   68 64 13

                   temperature-to-humidity map:
                   0 69 1
                   1 0 69

                   humidity-to-location map:
                   60 56 37
                   56 93 4
                   """;
        var parsed = Calc.Parse(data);
        var result = parsed.Map(Calc.Calc2);
        result.Map(x => x.Should().Be(46));
    }
    [Fact]
    public void Result2()
    {
        var parsed = Calc.Parse(File.ReadAllText("/Users/olegnykolyn/github/aoc/aoc2023/Day5/Data.txt"));
        var result = parsed.Map(Calc.Calc2);
        result.IsRight.Should().BeTrue();
        result.Map(x => x.Item1.Should().Be(13));
    }
}