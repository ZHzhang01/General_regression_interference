def min_edges_to_connect(adj_matrix, i, j):
    n = len(adj_matrix)
    visited = [False] * n
    
    def dfs(node, target, edges):
        if node == target:
            return edges
        visited[node] = True
        for k in range(n):
            if visited[k] == False and adj_matrix[node][k] > 0:
                result = dfs(k, target, edges + 1)
                if result > 0:
                    return result
        return 0
    
    result = dfs(i, j, 0)
    
    for k in range(n):
        visited[k] = False
    
    return result


# 举例
import random

# 生成一个100*100的矩阵
adj_matrix = [[random.choice([0, 1]) for _ in range(1000)] for _ in range(1000)]

# 打印矩阵
print("adj_matrix = [")
for row in adj_matrix:
    print(row, ",")
print("]")


i = 1
j = 30
result = min_edges_to_connect(adj_matrix, i, j)
print(f"节点{i}和节点{j}之间至少需要{result}条边才能取得连接")
