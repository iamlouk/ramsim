package main

import (
    "fmt"
    "os"
    "bufio"
    "strconv"
    "strings"
)

type raminstruction func()
type errors struct { reason string }
func (e errors) Error() string { return e.reason }

type registers struct {
    regs []int
}
func (regs *registers) Get(i int) int {
    if i >= len(regs.regs) {
        return 0
    } else {
        return regs.regs[i]
    }
}
func (regs *registers) Set(i, value int) {
    if i >= len(regs.regs) {
        regs.regs = append([]int(nil), regs.regs[:i + 10]...)
    }
    regs.regs[i] = value
}

var pc = 0
var regs registers
var instructions []raminstruction
var marks map[string]int

func perror(format string, a ...interface{}) {
    fmt.Fprintf(os.Stderr, format, a...)
    os.Exit(1)
}

func usage() {
    fmt.Printf("usage: %s <ramfile> [c(1), c(2), c(3), ...]\n", os.Args[0])
    fmt.Println("Sntax:")
    fmt.Println("\t<label>:")
    fmt.Println("\tSTORE       <n>")
    fmt.Println("\t[C|IND]LOAD <n>")
    fmt.Println("\t[C|IND]ADD  <n>")
    fmt.Println("\t[C|IND]SUB  <n>")
    fmt.Println("\t[C|IND]MUL  <n>")
    fmt.Println("\t[C|IND]DIV  <n>")
    fmt.Println("\tDUMP        <n> # dump n first registers")
    fmt.Println("\tGOTO <label>")
    fmt.Println("\tEND")
    fmt.Println("\tIF [=|!=|<|>|<=|>=] <n> GOTO <label>")
    os.Exit(1)
}

func parseNumber(field string) (int, error) {
    base := 10
    if len(field) > 1 && field[0] == '0' {
        if field[1] == 'x' {
            base = 16
        } else if field[1] == 'o' {
            base = 8
        } else if field[1] == 'b' {
            base = 2
        } else {
            return 0, errors{ reason: "invalid number litteral" }
        }
        field = field[2:]
    }
    n, err := strconv.ParseInt(field, base, 64)
    return int(n), err
}

func parseIf(fields []string) (raminstruction, error) {
    if len(fields) != 5 || fields[0] != "IF" || fields[3] != "GOTO" {
        return nil, errors{ reason: "IF [=|!=|<|>|<=|>=] <n> GOTO <label>" }
    }
    n, err := parseNumber(fields[2])
    if err != nil { return nil, err }

    op, mark := fields[1], fields[4]

    var condition func()bool
    switch op {
    case "=":  condition = func() bool { return regs.Get(0) == n }
    case "!=": condition = func() bool { return regs.Get(0) != n }
    case "<":  condition = func() bool { return regs.Get(0) < n  }
    case ">":  condition = func() bool { return regs.Get(0) > n  }
    case "<=": condition = func() bool { return regs.Get(0) <= n }
    case ">=": condition = func() bool { return regs.Get(0) >= n }
    default:
        return nil, errors{ reason: "IF [=|!=|<|>|<=|>=] <n> GOTO <label>" }
    }

    cmd := func(){
        if condition() {
            next, ok := marks[mark]
            if !ok {
                perror("error in IF: '%s' is not an existing label\n", mark)
            }
            pc = next - 1
        }
    }
    return cmd, nil
}

func getcommand(field string, n int) raminstruction {
    switch field {
    case "DUMP": return func(){ dump(n) }

    case "STORE": return func(){ regs.Set(n, regs.Get(0)) }
    case "INDSTORE": return func(){ regs.Set(regs.Get(n), regs.Get(0)) }

    case "LOAD": return func(){ regs.Set(0, regs.Get(n)) }
    case "CLOAD": return func(){ regs.Set(0, n) }
    case "INDLOAD": return func(){ regs.Set(0, regs.Get(regs.Get(n))) }

    case "ADD": return func(){ regs.Set(0, regs.Get(0) + regs.Get(n)) }
    case "CADD": return func(){ regs.Set(0, regs.Get(0) + n) }
    case "INDADD": return func(){ regs.Set(0, regs.Get(0) + regs.Get(regs.Get(n))) }

    case "SUB": return func(){ regs.Set(0, regs.Get(0) - regs.Get(n)) }
    case "CSUB": return func(){ regs.Set(0, regs.Get(0) - n) }
    case "INDSUB": return func(){ regs.Set(0, regs.Get(0) - regs.Get(regs.Get(n))) }

    case "MUL": return func(){ regs.Set(0, regs.Get(0) * regs.Get(n)) }
    case "CMUL": return func(){ regs.Set(0, regs.Get(0) * n) }
    case "INDMUL": return func(){ regs.Set(0, regs.Get(0) * regs.Get(regs.Get(n))) }

    case "DIV": return func(){ regs.Set(0, regs.Get(0) / regs.Get(n)) }
    case "CDIV": return func(){ regs.Set(0, regs.Get(0) / n) }
    case "INDDIV": return func(){ regs.Set(0, regs.Get(0) / regs.Get(regs.Get(n))) }
    }
    return nil
}

func dump(max int) {
    fmt.Printf("--- DUMP (PC: %d) ---\n", pc)
    for i := 0; i < max; i++ {
        fmt.Printf("c(%d): \t%d\n", i, regs.Get(i))
    }
}

func main() {
    if len(os.Args) < 2 {
        usage()
    }

    file, err := os.Open(os.Args[1])
    if err != nil {
        perror("open %s: %s\n", file, err.Error())
    }

    scanner := bufio.NewScanner(file)
    scanner.Split(bufio.ScanLines)

    marks = make(map[string]int)
    regs = registers{ regs: make([]int, 1024) }
    instructions = make([]raminstruction, 0)

    for regi, argi := 1, 2; argi < len(os.Args); regi, argi = regi + 1, argi + 1 {
        val, err := parseNumber(os.Args[argi])
        if err != nil {
            perror("argument cannot be converted to a number: '%s'\n", os.Args[argi])
        }
        regs.Set(regi, val)
    }

    for ic, line := 0, 1; scanner.Scan(); line++ {
        text := scanner.Text()
        if index := strings.Index(text, "#"); index >= 0 {
            text = text[:index]
        }

        fields := strings.Fields(text)
        if len(fields) == 0 { continue }

        if strings.HasSuffix(fields[0], ":") {
            mark := fields[0][0:len(fields[0]) - 1]
            marks[mark] = ic
            fields = fields[1:]
            if len(fields) == 0 { continue }
        }

        switch fields[0] {
        case "IF":
            cmd, err := parseIf(fields)
            if err != nil {
                perror("syntax error (line %d): %s\n", line, err.Error())
            }
            instructions = append(instructions, cmd)
        case "END":
            if len(fields) != 1 {
                perror("syntax error (line %d): END\n", line)
            }
            instructions = append(instructions, func(){
                fmt.Printf("--- END ---\nc(0) = %d\n", regs.Get(0))
                os.Exit(0)
            })
        case "GOTO":
            mark := fields[1]
            if len(fields) != 2 {
                perror("syntax error (line %d): GOTO <label>\n", line)
            }
            instructions = append(instructions, func(){
                next, ok := marks[mark]
                if !ok {
                    perror("error in GOTO: '%s' is not an existing label\n", mark)
                }
                pc = next - 1
            })
        default:
            if len(fields) != 2 {
                perror("syntax error (line %d): INSTRUCTION <n>\n", line)
            }
            n, err := parseNumber(fields[1])
            if err != nil { panic(err) }
            if cmd := getcommand(fields[0], n); cmd != nil {
                instructions = append(instructions, cmd)
            } else {
                perror("syntax error (line %d): unkown instruction '%s'\n", line, fields[0])
            }
        }

        ic++
    }

    file.Close()

    for pc < len(instructions) {
        cmd := instructions[pc]
        cmd()
        pc++
    }

    fmt.Fprintf(os.Stderr, "no END!\nc(0) = %d\n", regs.Get(0))
    os.Exit(1)
}
