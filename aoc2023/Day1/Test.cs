using Xunit;

namespace aoc2023.Day1;

public class Test
{
    [Fact]
    public void TestCaseSample1()
    {
        var data = """
                   1abc2
                   pqr3stu8vwx
                   a1b2c3d4e5f
                   treb7uchet
                   """;
        Assert.Equal(142, Calc.Calculate(data.Split(new[] { '\n' })));
    }

    [Fact]
    public void TestCaseSample2()
    {
        var data = """
                   two1nine
                   eightwothree
                   abcone2threexyz
                   xtwone3four
                   4nineeightseven2
                   zoneight234
                   7pqrstsixteen
                   """;
        Assert.Equal(281, Calc.Calculate(data.Split(new[] { '\n' })));
    }

    [Fact(Skip = "For getting an answer")]
    public void TestCaseSample3()
    {
        Assert.Equal(0, Calc.Calculate(File.ReadLines("/Users/olegnykolyn/github/aoc/aoc2023/Day1/Data.txt")));
    }
}