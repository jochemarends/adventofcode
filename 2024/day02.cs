namespace AdventOfCode
{
    class Solution
    {
        static bool IsSuperSafe(IEnumerable<int> report)
        {
            var gaps = report.Skip(1).Zip(report, (a, b) => a - b);
            var signs = gaps.Select(Math.Sign);
            return gaps.Select(Math.Abs).All(n => n >= 1 && n <= 3) 
                && signs.Skip(1).All(n => n == signs.First());
        }

        static bool IsSafe(IEnumerable<int> report)
            => report.Select((_, index) => report.Where((_, idx) => index != idx))
                .Any(IsSuperSafe);

        static int Part1(IEnumerable<IEnumerable<int>> input)
            => input.Where(IsSuperSafe).Count();

        static int Part2(IEnumerable<IEnumerable<int>> input)
            => input.Where(IsSafe).Count();

        static IEnumerable<IList<int>> Parse(StreamReader stream)
        {
            string? line;
            while ((line = stream.ReadLine()) != null) 
            {
                yield return line.Split(' ').Select(Int32.Parse).ToList();
            }
        }

        static void Main()
        {
            var input = Parse(new StreamReader("./input.txt")).ToList();
            Console.WriteLine($"part 1: {Part1(input)}");
            Console.WriteLine($"part 2: {Part2(input)}");
        }
    }
}
