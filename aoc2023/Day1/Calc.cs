namespace aoc2023.Day1;

public class Calc
{
    public static int Calculate(IEnumerable<string> data)
    {
        Func<string,int> getDigits = s =>
        {
            List<(string, string)> replaceRules = new()
            {
                ("one", "1"),
                ("two", "2"),
                ("three", "3"),
                ("four", "4"),
                ("five", "5"),
                ("six", "6"),
                ("seven", "7"),
                ("eight", "8"),
                ("nine", "9"),
            };
            foreach ((var from, var to) in replaceRules)
            {
                s = s.Replace(from, $"{from}{to}{from}");
            }
            var filtered = s.Where(c => c >= '0' && c <= '9').ToList();
            return int.Parse($"{filtered.First().ToString()}{filtered.Last().ToString()}");
        };
        return data.Select(getDigits).Sum();
    }
}