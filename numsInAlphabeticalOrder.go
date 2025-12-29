package main

import (
	"fmt"
	"math"
	"slices"
	"strings"
)

const nMax = 100
const nMin = 0

const lbreak = 12 //int >= 1

//This version only works for integers from 0 to 100

func numberToWords(n int) string { //can only take 0 to 100
	if n > 100 || n < 0 {
		return strings.Repeat("z", math.MaxInt) //out of bounds
	} else if n == 0 {
		return "zero"
	} else if n == 100 {
		return "one hundred"
	}
	umap := map[int]string{
		0: "", //we don't say zero
		1: "one",
		2: "two",
		3: "three",
		4: "four",
		5: "five",
		6: "six",
		7: "seven",
		8: "eight",
		9: "nine",
	} //units

	tmap := map[int]string{
		0: "",
		2: "twenty",
		3: "thirty",
		4: "forty",
		5: "fifty",
		6: "sixty",
		7: "seventy",
		8: "eighty",
		9: "ninty",
	} //tens

	d10 := map[int]string{
		0: "ten",
		1: "eleven",
		2: "twelve",
		3: "thirteen",
		4: "fourteen",
		5: "fifteen",
		6: "sixteen",
		7: "seventeen",
		8: "eighteen",
		9: "nineteen",
	} //Dictionary for 11-20

	t := n / 10
	u := n % 10
	if t == 0 {
		return umap[u]
	} else if t == 1 {
		return d10[u]
	}
	return fmt.Sprintf("%s %s", tmap[t], umap[u])
}

func main() {
	//input limits here
	left := 10
	right := 100

	//buffer with internal limits
	left = max(nMin, left)
	right = min(nMax, right)

	fmt.Printf("Here are the numbers from %d to %d in alphabetical order:\n", left, right)

	nums := make([]int, 0)
	for n := left; n <= right; n++ {
		nums = append(nums, n)
	}
	slices.SortFunc(nums, func(a, b int) int {
		return strings.Compare(numberToWords(a), numberToWords(b))
	})
	for i, n := range nums {
		if i < len(nums)-1 {
			fmt.Printf("%d, ", n)
			if i%lbreak == lbreak-1 {
				fmt.Printf("\n")
			}
		} else {
			fmt.Printf("%d", n)
		}
	}
}
