function factorial(n) {
  let result = 1
  for (let i = 1; i <= n; i = i + 1) {
    result = result * i
  }
  return result
}
factorial(5)