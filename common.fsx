let splitStringInLines (s:string) = s.Split([|System.Environment.NewLine|],System.StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x->x.Trim())
let splitStringInLinesKeepEmpty (s:string) = s.Split([|System.Environment.NewLine|],System.StringSplitOptions.None) |> Array.map (fun x->x.Trim())
let splitOnSpaces (s:string) = s.Split([|' '|],System.StringSplitOptions.None) |> Array.map (fun x->x.Trim())

let sLength (s:string) = if System.String.IsNullOrWhiteSpace s then 0 else s.Length
let hasLength len s = (sLength s) = len

let isInt x = System.Int32.TryParse x |> fst
let isLong x = System.Int64.TryParse x |> fst
let isDigit c = ('0'<=c && c<='9')