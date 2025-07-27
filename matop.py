class Matrix:
	def __init__(self, rows, cols, data=None):
		if rows <= 0 or cols <= 0:
			raise ValueError(f'Invalid Matrix Dimensions: `{rows}x{cols}`')
		self.rows = rows
		self.cols = cols
		if data is not None:
			if len(data) != rows or any(len(row) != cols for row in data):
				raise ValueError(f'Matrix Dimension Mismatch:\n\tExpected: `{rows}x{cols}` Got: `{len(data)}x{len(data[0])}`')
			self.data = data
		else:
			self.data = [[0.0 for _ in range(cols)] for _ in range(rows)]
	
	def check_dims(self, other):
		if self.rows != other.rows or self.cols != other.cols:
			raise ValueError(f'Matrices Dimensions Mismatch `{self.rows}x{self.cols}` and `{other.rows}x{other.cols}`')

	def add(self, other):
		self.check_dims(other=other)
		tmp = Matrix(self.rows, self.cols)
		for i in range(self.rows):
			for j in range(self.cols):
				tmp.data[i][j] = self.data[i][j] + other.data[i][j]
		return tmp

	def sub(self, other):
		self.check_dims(other=other)
		tmp = Matrix(self.rows, self.cols)
		for i in range(self.rows):
			for j in range(self.cols):
				tmp.data[i][j] = self.data[i][j] - other.data[i][j]
		return tmp

	def mul(self, other):
		tmp = Matrix(self.rows, other.cols)
		if isinstance(other, (int, float)):
			for i in range(self.rows):
				for j in range(self.cols):
					tmp.data[i][j] = self.data[i][j] * other
		elif isinstance(other, Matrix):
			if self.cols != other.rows:
				raise ValueError(f'Matrices Dimensions Mismatch for Multiplication `{self.rows}x{self.cols}` and `{other.rows}x{other.cols}`')
			for i in range(self.rows):
				for j in range(other.cols):
					tmp.data[i][j] = sum(self.data[i][k] * other.data[k][j] for k in range(self.cols))
		return tmp

	def transpose(self):
		tmp = Matrix(self.rows, self.cols)
		for i in range(self.rows):
			for j in range(self.cols):
				tmp.data[j][i] = self.data[i][j]
		return tmp

	def determinant(self):
		if self.rows != self.cols:
			raise ValueError('Determinant only computed for Square Matrix')
		if self.rows == 1:
			return self.data[0][0]
		if self.rows == 2:
			return (self.data[0][0] * self.data[1][1]) - (self.data[0][1] * self.data[1][0])

		det = 0.0
		for j in range(self.cols):
			submat = Matrix(self.rows-1, self.cols-1)
			for r in range(1, self.rows):
				for c in range(self.cols):
					if c < j:
						submat.data[r-1][c] = self.data[r][c]
					elif c > j:
						submat.data[r-1][c-1] = self.data[r][c]
			det += pow(-1, j) * self.data[0][j] * submat.determinant()
		return det

	def identity(self, rows, cols):
		if rows != cols:
			raise ValueError('Identiy Matrix must be Square Matrix')
		tmp = Matrix(rows, cols)
		for i in range(rows):
			for j in range(cols):
				tmp.data[i][j] = 1.0 if i == j else 0.0
		return tmp

	def __str__(self) -> str:
		return '\n\t'.join([' '.join(map(str, row)) for row in self.data])

# mat1 = Matrix(2, 2, [[2,4], [3,5]])
# print(f'mat1:\n\t{mat1}\n')

# mat2 = Matrix(2, 3, [[2,4,6], [3,5,7]])
# print(f'mat2:\n\t{mat2}\n')

# mat3 = mat1.mul(mat2)
# print(f'mat1 * mat2:\n\t{mat3}')
