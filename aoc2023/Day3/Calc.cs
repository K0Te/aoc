namespace aoc2023.Day3;

public record Part(char Symbol, int Coord);

public record PartNumber(int Value, int Coord);

public record Board(IEnumerable<Part> Parts, IEnumerable<PartNumber> PartNumbers, int LineLen);

public class Calc
{
    public static Board Parse(IEnumerable<string> ss)
    {
        var s = string.Join("", ss);
        List<PartNumber> partNumbers = new();
        List<Part> parts = new();
        var pos = 0;
        while (pos < s.Length)
        {
            if (s[pos] == '.')
            {
                pos++;
            }
            else if (s[pos] <= '9' && s[pos] >= '0')
            {
                var len = 0;
                var res2 = 0;
                while (int.TryParse(s.Substring(pos, len + 1), out var res))
                {
                    len++;
                    res2 = res;
                }

                partNumbers.Add(new PartNumber(res2, pos));
                pos += len;
            }
            else
            {
                parts.Add(new Part(s[pos], pos));
                pos++;
            }
        }

        return new Board(parts, partNumbers, ss.First().Length);
    }

    public static bool PartNumIsAdjacent(Board board, PartNumber partNumber)
    {
        var parNumberCoords = Enumerable.Range(partNumber.Coord, partNumber.Value.ToString().Length);
        var isAjd = (int x1, int x2) => x1 switch
        {
            // End of line
            (var n) when n % board.LineLen == (board.LineLen - 1) => x1 - x2 == 1 ||
                                                                     Math.Abs(x1 - x2) == board.LineLen ||
                                                                     Math.Abs(x1 - x2) == board.LineLen - 1,
            // Start of line
            (var n) when n % board.LineLen == 0 => x1 - x1 == 1 || Math.Abs(x2 - x1) == board.LineLen ||
                                                   Math.Abs(x1 - x2) == board.LineLen + 1,
            // Else
            (var n) =>
                Math.Abs(x1 - x2) == 1 ||
                Math.Abs(x1 - x2) == board.LineLen ||
                Math.Abs(x1 - x2) == board.LineLen - 1 ||
                Math.Abs(x1 - x2) == board.LineLen + 1
        };
        return board.Parts.Any(p => parNumberCoords.Any(pn => isAjd(p.Coord, pn)));
    }
    public static IEnumerable<HashSet<PartNumber>> GetGears(Board board)
    {
        var isAjd = (int x1, int x2) => x1 switch
        {
            // End of line
            (var n) when n % board.LineLen == (board.LineLen - 1) => x1 - x2 == 1 ||
                                                                     Math.Abs(x1 - x2) == board.LineLen ||
                                                                     Math.Abs(x1 - x2) == board.LineLen - 1,
            // Start of line
            (var n) when n % board.LineLen == 0 => x1 - x1 == 1 || Math.Abs(x2 - x1) == board.LineLen ||
                                                   Math.Abs(x1 - x2) == board.LineLen + 1,
            // Else
            (var n) =>
                Math.Abs(x1 - x2) == 1 ||
                Math.Abs(x1 - x2) == board.LineLen ||
                Math.Abs(x1 - x2) == board.LineLen - 1 ||
                Math.Abs(x1 - x2) == board.LineLen + 1
        };

        var getMatches = (Part part) =>
        {
            HashSet<PartNumber> res = new();
            foreach (var partNumber in board.PartNumbers)
            {
                var parNumberCoords = Enumerable.Range(partNumber.Coord, partNumber.Value.ToString().Length);
                if (parNumberCoords.Any(pnc => isAjd(pnc, part.Coord)))
                {
                    res.Add(partNumber);
                }
            }

            return res;
        };
        return board.Parts.Where(part => part.Symbol == '*').Select(p => getMatches(p)).Where(res => res.Count == 2);
    }

    public static int Calculate(IEnumerable<string> ss)
    {
        var board = Parse(ss);
        return board.PartNumbers.Where(p => PartNumIsAdjacent(board, p)).Sum(p => p.Value);
    }
    public static int Calculate2(IEnumerable<string> ss)
    {
        var board = Parse(ss);
        return GetGears(board).Select(set => set.Aggregate(1, (x, y) => x * y.Value)).Sum();
    }
}