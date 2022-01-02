using System.Collections.Immutable;
using FluentAssertions;
using Xunit;

namespace aoc2023.Day3;

public class Test
{
    [Fact]
    public void Parsing()
    {
        var data = """
                   467..114..
                   ...*......
                   ..35..633.
                   ......#...
                   617*......
                   .....+.58.
                   ..592.....
                   ......755.
                   ...$.*....
                   .664.598..
                   """;
        var result = Calc.Parse(data.Split(new []{'\n'}));
        result.PartNumbers.Should().HaveCount(10);
        result.Parts.Should().HaveCount(6);
    }
    
    [Fact]
    public void Sample1()
    {
        var data = """
                   467..114..
                   ...*......
                   ..35..633.
                   ......#...
                   617*......
                   .....+.58.
                   ..592.....
                   ......755.
                   ...$.*....
                   .664.598..
                   """;
        var result = Calc.Calculate(data.Split(new []{'\n'}));
        result.Should().Be(4361);
    }
    
    [Fact(Skip = "For getting an answer")]
    public void Result1()
    {
        var result = Calc.Calculate(File.ReadLines("/Users/olegnykolyn/github/aoc/aoc2023/Day3/Data.txt"));
        result.Should().Be(8);
    }
    
    [Fact]
    public void Sample2()
    {
        var data = """
                   467..114..
                   ...*......
                   ..35..633.
                   ......#...
                   617*......
                   .....+.58.
                   ..592.....
                   ......755.
                   ...$.*....
                   .664.598..
                   """;
        var result = Calc.Calculate2(data.Split(new []{'\n'}));
        result.Should().Be(467835);
    }
    
    [Fact]
    public void Result2()
    {
        var result = Calc.Calculate2(File.ReadLines("/Users/olegnykolyn/github/aoc/aoc2023/Day3/Data.txt"));
        result.Should().Be(8);
    }
}