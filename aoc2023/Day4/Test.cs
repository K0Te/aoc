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

namespace aoc2023.Day4;

public class Test
{
    [Fact]
    public void Parsing()
    {
        var data = """
                   Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                   Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                   Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                   Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                   Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                   Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
                   """;
        var result = Calc.Parse(data.Split(new []{'\n'}));
        result.IsRight.Should().BeTrue();
        result.Map(rs => rs.First().Should().Be(
             new Line(1, new []{41, 48, 83, 86, 17}, new []{83, 86,  6, 31, 17,  9, 48, 53})));
    }
    
    [Fact]
    public void Sample1()
    {
        var data = """
                   Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                   Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                   Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                   Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                   Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                   Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
                   """;
        var parsed = Calc.Parse(data.Split(new []{'\n'}));
        var result = parsed.Map(Calc.Calc1);
        result.Map(x => x.Should().Be(13));
    }
    
    [Fact(Skip = "For getting an answer")]
    public void Result1()
    {
        var parsed = Calc.Parse(File.ReadLines("/Users/olegnykolyn/github/aoc/aoc2023/Day4/Data.txt"));
        var result = parsed.Map(Calc.Calc1);
        result.IsRight.Should().BeTrue();
        result.Map(x => x.Should().Be(13));
    }
    [Fact]
    public void Sample2()
    {
        var data = """
                   Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                   Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                   Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                   Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                   Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                   Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
                   """;
        var parsed = Calc.Parse(data.Split(new []{'\n'}));
        var result = parsed.Map(Calc.Calc2);
        result.Map(x => x.Should().Be(30));
    }
    [Fact]
    public void Result2()
    {
        var parsed = Calc.Parse(File.ReadLines("/Users/olegnykolyn/github/aoc/aoc2023/Day4/Data.txt"));
        var result = parsed.Map(Calc.Calc2);
        result.IsRight.Should().BeTrue();
        result.Map(x => x.Should().Be(13));
    }

}