using System.Collections.Immutable;
using FluentAssertions;
using Xunit;

namespace aoc2023.Day2;

public class Test
{
    [Fact]
    public void Parsing()
    {
        var data = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue";
        var result = Calc.ParseGame(data);
        List<IEnumerable<(int, string)>> expectedArray = new()
        {
            new List<(int, string)>()
            {
                (1, "blue"),
                (2, "green"),
            },
            new List<(int, string)>()
            {
                (3, "green"),
                (4, "blue"),
                (1, "red"),
            },
            new List<(int, string)>()
            {
                (1, "green"),
                (1, "blue"),
            },
        };
        Assert.Equal(2, result.Id);
        result.Draws.Should().BeEquivalentTo(expectedArray);
    }

    [Fact]
    public void Sample1()
    {
        var data = """
                   Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                   Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
                   Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
                   Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
                   Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
                   """;
        Dictionary<string, int> conf = new()
        {
            { "red", 12 },
            { "green", 13 },
            { "blue", 14 },
        };
        var result = Calc.Calculate(data.Split(new[] { '\n' }), conf);
        result.Should().Be(8);
    }

    [Fact(Skip = "For getting an answer")]
    public void Result1()
    {
        Dictionary<string, int> conf = new()
        {
            { "red", 12 },
            { "green", 13 },
            { "blue", 14 },
        };
        var result = Calc.Calculate(File.ReadLines("/Users/olegnykolyn/github/aoc/aoc2023/Day2/Data.txt"), conf);
        result.Should().Be(8);
    }
    
    [Fact]
    public void Sample2()
    {
        var data = """
                   Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                   Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
                   Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
                   Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
                   Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
                   """;
        var result = Calc.Calculate2(data.Split(new[] { '\n' }));
        result.Should().Be(2286);
    }
    
    [Fact(Skip = "For getting an answer")]
    public void Result2()
    {
        var result = Calc.Calculate2(File.ReadLines("/Users/olegnykolyn/github/aoc/aoc2023/Day2/Data.txt"));
        result.Should().Be(8);
    }
}