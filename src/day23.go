package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"log"
	"os"
	"runtime"
	"runtime/pprof"
	"strconv"
	"strings"
	"time"
)

type Operation int

const (
	Increase Operation = iota
	Decrease
	Toggle
	Copy
	Jump
)

func OpFromString(s string) Operation {
	switch s {
	case "inc":
		return Increase
	case "dec":
		return Decrease
	case "tgl":
		return Toggle
	case "cpy":
		return Copy
	case "jnz":
		return Jump
	}
	panic("OpFromString(" + s + ")")
}

type Register struct {
	vs [4]int
}

func (r Register) get(pos byte) int {
	return r.vs[pos]
}

func (r *Register) set(pos byte, val int) {
	r.vs[pos] = val
}

func (r *Register) inc(pos byte) {
	r.vs[pos]++
}

func (r *Register) dec(pos byte) {
	r.vs[pos]--
}

func valueOf(vol ValueOrLocation, r Register) int {
	if vol.IsLocation {
		return r.get(vol.Location)
	}
	return vol.Value
}

type ValueOrLocation struct {
	IsLocation bool
	Location   byte
	Value      int
}

func FromString(s string) ValueOrLocation {
	switch s {
	case "a":
		return ValueOrLocation{Location: 0, IsLocation: true}
	case "b":
		return ValueOrLocation{Location: 1, IsLocation: true}
	case "c":
		return ValueOrLocation{Location: 2, IsLocation: true}
	case "d":
		return ValueOrLocation{Location: 3, IsLocation: true}
	default:
		v, err := strconv.Atoi(s)
		if err != nil {
			panic("FromString(" + s + "):" + err.Error())
		}
		return ValueOrLocation{
			Value: v,
		}
	}
}

type Expr struct {
	operation Operation
	x, y      ValueOrLocation
}

func parseLine(line string) (*Expr, error) {
	splitted := strings.Split(line, " ")
	if len(splitted) == 2 {
		return &Expr{
			operation: OpFromString(splitted[0]),
			x:         FromString(splitted[1]),
		}, nil
	}
	if len(splitted) == 3 {
		return &Expr{
			operation: OpFromString(splitted[0]),
			x:         FromString(splitted[1]),
			y:         FromString(splitted[2]),
		}, nil
	}
	return nil, errors.New("unexpected line format")
}

func inc(x int) int { return x + 1 }
func dec(x int) int { return x - 1 }

func execute(e *Expr, r *Register, i int, exprs []*Expr) int {
	switch e.operation {
	case Increase:
		// if e.x.IsLocation {
		// 	r.inc(e.x.Location)
		// }
		r.inc(e.x.Location)
		return i + 1
	case Decrease:
		// if e.x.IsLocation {
		// 	r.dec(e.x.Location)
		// }
		r.dec(e.x.Location)
		return i + 1
	case Toggle:
		// idx := i + valueOf(e.x, *r)
		idx := i + r.get(e.x.Location)
		if idx < len(exprs) {
			switch exprs[idx].operation {
			case Increase:
				exprs[idx].operation = Decrease
			case Decrease, Toggle:
				exprs[idx].operation = Increase
			case Copy:
				exprs[idx].operation = Jump
			case Jump:
				exprs[idx].operation = Copy
			}
		}
		return i + 1
	case Copy:
		if e.y.IsLocation {
			v := e.x.Value
			if e.x.IsLocation {
				v = r.get(e.x.Location)
			}
			r.set(e.y.Location, v)
		}
		return i + 1
	case Jump:
		v := e.x.Value
		if e.x.IsLocation {
			v = r.get(e.x.Location)
		}
		if v != 0 {
			return i + valueOf(e.y, *r)
		}
		return i + 1
	}

	panic("execute")
}

func solve(a int, exprs []*Expr) int {
	r := &Register{[4]int{a, 0, 0, 0}}
	index := 0
	maxIndex := len(exprs)
	for index < maxIndex {
		index = execute(exprs[index], r, index, exprs)
		//printexprs(index, exprs)
	}
	return r.get(0)
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
	var memprofile = flag.String("memprofile", "", "write mem profile to `file`")
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

	if *memprofile != "" {
		f, err := os.Create(*memprofile)
		if err != nil {
			log.Fatal("could not create memory profile: ", err)
		}
		defer f.Close() // error handling omitted for example
		runtime.GC()    // get up-to-date statistics
		if err := pprof.WriteHeapProfile(f); err != nil {
			log.Fatal("could not write memory profile: ", err)
		}
	}
}
