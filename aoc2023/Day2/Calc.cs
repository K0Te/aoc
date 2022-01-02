namespace aoc2023.Day2;

using Draw = IEnumerable<(int, string)>;
using Config = Dictionary<string, int>;

public record Game(int Id, IEnumerable<Draw> Draws);

public class Calc
{
    public static Draw ParseDraw(string drawString)
    {
        foreach (var data in drawString.Split(","))
        {
            var dataTrimmed = data.Trim();
            if (dataTrimmed.Split(" ") is [string count, string colour])
            {
                yield return ((int.Parse(count), colour));
            }
        }
    }
    public static Game ParseGame(string s)
    {
        var parts = s.Split(":");
        var gameId = int.Parse(parts.First().Substring("Game ".Length));
        List<Draw> draws = new();
        foreach (var drawString in parts.Last().Split(";"))
        {
            draws.Add(ParseDraw(drawString));
        }

        return new Game(gameId, draws);
    }
    public static bool IsDrawPossible(Draw draw, Config cong)
    {
        return draw.All(Predicate);

        bool Predicate((int, string) arg)
        {
            (var count, var colour) = arg;
            return cong.GetValueOrDefault(colour, 0) >= count;
        }
    }
    public static int Calculate(IEnumerable<string> data, Config conf)
    {
        var games = data.Select(ParseGame);
        return games.Where(g => g.Draws.All(d => IsDrawPossible(d, conf))).Sum(g => g.Id);
    }

    public static int GamePower(Game g)
    {
        var minByColor = (string c) => g.Draws
            .Select(x =>
                x.FirstOrDefault(
                    tuple => tuple.Item2 == c,
                    (0, c)
                    ).Item1).Max();
        return minByColor("red") * minByColor("green") * minByColor("blue");
    }
    
    public static int Calculate2(IEnumerable<string> data)
    {
        var games = data.Select(ParseGame);
        return games.Select(GamePower).Sum();
    }
}