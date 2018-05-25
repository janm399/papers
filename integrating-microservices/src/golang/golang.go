package main

import ("strconv"
	"time"
	"fmt"
	"errors"
	"os"
	"net/http"
)

func fb1(i int) string {
	if i % 3 == 0 && i % 5 == 0 {
		return "FizzBuzz"
	} else if i % 3 == 0 {
		return "Fizz"
	} else if i % 5 == 0 {
		return "Buzz"
	} else {
		return strconv.Itoa(i)
	}
}

func fb2aa(i int) (res string, err error) {
	if i % 3 == 0 && i % 5 == 0 {
		return "", errors.New("FizzBuzz")
	} else if i % 3 == 0 {
		return "Fizz", nil
	} else if i % 5 == 0 {
		return "Buzz", nil
	} else {
		return strconv.Itoa(i), nil
	}
}

func fb2bb(i int) (res string, err error) {
	if i % 3 == 0 && i % 5 == 0 {
		return "", errors.New("FizzBuzz")
	} else if i % 3 == 0 {
		return "", errors.New("Fizz")
	} else if i % 5 == 0 {
		return "", errors.New("Buzz")
	} else {
		return "", errors.New(strconv.Itoa(i))
	}
}

func fb2a(i int) string {
	r, e := fb2aa(i)
	if e != nil {
		return e.Error()
	} else {
		return r
	}
}

func fb2b(i int) string {
	_, e := fb2bb(i)
	return e.Error()
}

type fizz interface {
	Fizz() bool
}

type buzz interface {
	Buzz() bool
}

type fizzBuzz interface {
	FizzBuzz() bool
}

type T int
func (t T) Fizz() bool {
	return t % 3 == 0
}
func (t T) Buzz() bool {
	return t % 5 == 0
}
func (t T) FizzBuzz() bool {
	return t.Fizz() && t.Buzz()
}

func fb3(i int) string {
	t := T(i)
	if t.FizzBuzz() {
		return "FizzBuzz"
	} else if t.Fizz() {
		return "Fizz"
	} else if t.Buzz() {
		return "Buzz"
	} else {
		return strconv.Itoa(i)
	}
}

func fb4(i int) string {
	in := make(chan int)
	out := make(chan string)
	go (func() {
		x := <- in
		out <- fb1(x)
	})()
	in <- i
	return <- out
}

func withIO(fn string, f func(int) string) (func(int) string) {
	return func(n int) string {
		file, _ := os.OpenFile(fn, os.O_RDONLY, 0)
		bytes := make([]byte, 1024*1024)
		file.Read(bytes)
		file.Close()
		return f(n)
	}
}

func withHttp(url string, f func(int) string) (func(int) string) {
	tr := &http.Transport{
		MaxIdleConns:       10,
		IdleConnTimeout:    30 * time.Second,
		DisableCompression: true,
	}
	client := &http.Client{Transport: tr}
	return func(n int) string {
		resp, _:= client.Get(url)
		resp.Body.Close()
		return f(n)
	}
}

func timed(f func(int) string) int64 {
	start := time.Now()
	for i := 0; i < 4000000; i++ {
		f(i)
	}
	diff := time.Now().UnixNano() - start.UnixNano()
	fmt.Printf("Took %d\n", diff / 1000000)
	return diff
}

func main() {
	for i := 0; i < 10; i++ {
		timed(withHttp("http://localhost:8080", fb1))
		timed(fb1)
		timed(fb2a)
		timed(fb2b)
		timed(fb3)
		timed(fb4)
		timed(withIO("/dev/zero", fb1))
		timed(withIO("/tmp/zeros", fb1))
		fmt.Println()
		fmt.Println()
	}
}
