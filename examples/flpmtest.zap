@module FLPMTest

def sum [] := 0
def sum (x :: xs) := x + sum xs

def map _ [] := []
def map f (x :: xs) := f x :: map f xs
