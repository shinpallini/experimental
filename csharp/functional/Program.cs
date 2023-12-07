using System;
using System.Net.Http;
using System.Threading.Tasks;
using LanguageExt;
using static LanguageExt.Prelude;

public class HttpRequestExample
{
    public static async Task Main()
    {
        var httpRequestExample = new HttpRequestExample();
        var result = await httpRequestExample.FetchDataAsync("http://example.com");

        result.Match(
            Right: responseBody => Console.WriteLine("Success: " + responseBody),
            Left: errorMessage => Console.WriteLine("Error: " + errorMessage)
        );

    }
    public async Task<Either<string, string>> FetchDataAsync(string url)
    {
        using (var client = new HttpClient())
        {
            try
            {
                var response = await client.GetAsync(url);
                response.EnsureSuccessStatusCode();
                var responseBody = await response.Content.ReadAsStringAsync();
                return Right<string, string>(responseBody);
            }
            catch (HttpRequestException e)
            {
                return Left<string, string>(e.Message);
            }
        }
    }
}

