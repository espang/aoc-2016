package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"log"
	"os"
	"runtime/pprof"
	"strconv"
	"strings"
	"time"
)

type Register struct {
	a, b, c, d int
}

func (r Register) get(pos byte) int {
	switch pos {
	case 0:
		return r.a
	case 1:
		return r.b
	case 2:
		return r.c
	case 3:
		return r.d
	}
	panic("unexpected string")
}
func (r *Register) applyIfPossible(pos byte, f func(int) int) {
	switch pos {
	case 0:
		r.a = f(r.a)
	case 1:
		r.b = f(r.b)
	case 2:
		r.c = f(r.c)
	case 3:
		r.d = f(r.d)
	}
}

// ValueOrLocation has exactly one field set!
type ValueOrLocation struct {
	Location *byte
	Value    *int
}

type Expr struct {
	operation string
	x, y      ValueOrLocation
}

func parseLine(line string) (*Expr, error) {
	splitted := strings.Split(line, " ")
	if len(splitted) == 2 {
		return &Expr{operation: splitted[0], x: splitted[1]}, nil
	}
	if len(splitted) == 3 {
		return &Expr{operation: splitted[0], x: splitted[1], y: splitted[2]}, nil
	}
	return nil, errors.New("unexpected line format")
}

func valueOf(s string, r Register) int {
	switch s {
	case "a", "b", "c", "d":
		return r.get(s)
	default:
		v, err := strconv.Atoi(s)
		if err != nil {
			panic("valueOf(" + s + "):" + err.Error())
		}
		return v
	}
}

func inc(x int) int { return x + 1 }
func dec(x int) int { return x - 1 }

func execute(e *Expr, r *Register, i int, exprs []*Expr) int {
	switch e.operation {
	case "inc":
		r.applyIfPossible(e.x, inc)
		return i + 1
	case "dec":
		r.applyIfPossible(e.x, dec)
		return i + 1
	case "tgl":
		idx := i + valueOf(e.x, *r)
		if idx >= 0 && idx < len(exprs) {
			switch exprs[idx].operation {
			case "inc":
				exprs[idx].operation = "dec"
			case "dec", "tgl":
				exprs[idx].operation = "inc"
			case "cpy":
				exprs[idx].operation = "jnz"
			case "jnz":
				exprs[idx].operation = "cpy"
			}
		}
		return i + 1
	case "cpy":
		v := valueOf(e.x, *r)
		r.applyIfPossible(e.y, func(int) int { return v })
		return i + 1
	case "jnz":
		v := valueOf(e.x, *r)
		if v != 0 {
			return i + valueOf(e.y, *r)
		}
		return i + 1
	}

	panic("execute")
}

func step(r *Register, i int, exprs []*Expr) (int, bool) {
	if i >= 0 && i < len(exprs) {
		i = execute(exprs[i], r, i, exprs)
		return i, true
	}
	return 0, false
}

func solve(a int, exprs []*Expr) int {
	r := &Register{a, 0, 0, 0}
	next := true
	index := 0
	for next {
		index, next = step(r, index, exprs)
		//printexprs(index, exprs)
	}
	return r.a
}

func printexprs(i int, exprs []interface{}) {
	fmt.Printf("%09d |", i)
	for _, e := range exprs {
		fmt.Printf("%v|", e)
	}
	fmt.Println("")
}

func main() {
	var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to `file`")
	flag.Parse()
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal("could not create CPU profile: ", err)
		}
		defer f.Close() // error handling omitted for example
		if err := pprof.StartCPUProfile(f); err != nil {
			log.Fatal("could not start CPU profile: ", err)
		}
		defer pprof.StopCPUProfile()
	}

	f, err := os.Open("../input_23.txt")
	if err != nil {
		log.Fatal("readFile:", err)
	}

	s := bufio.NewScanner(f)
	var exprs []*Expr
	for s.Scan() {
		line := s.Text()
		expr, err := parseLine(line)
		if err != nil {
			log.Fatalf("parseLine(%s):%v", line, err)
		}
		exprs = append(exprs, expr)
	}

	if err := s.Err(); err != nil {
		log.Fatal("scanner:", err)
	}

	start := time.Now()
	//printexprs(0, exprs)
	solution := solve(12, exprs)

	fmt.Printf("took %v to solve it. Result is: %d", time.Since(start), solution)
	fmt.Println("")
}
