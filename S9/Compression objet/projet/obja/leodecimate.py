#!/usr/bin/env python

import obja
import numpy as np
import sys

class Decimater(obja.Model):
    """
    A simple class that decimates a 3D model stupidly.
    """
    def __init__(self):
        super().__init__()
        self.deleted_faces = set()

    def area(self, face):
        """
        Calculate the area of a triangle defined by its vertices in 3D space.
        'face' should be a structure containing the vertex indices (a, b, c).
        """
        vertex_a = self.vertices[face.a]
        vertex_b = self.vertices[face.b]
        vertex_c = self.vertices[face.c]

        x1, y1, z1 = vertex_a
        x2, y2, z2 = vertex_b
        x3, y3, z3 = vertex_c

        # Calculate the area using the formula
        triangle_area = 0.5 * abs(
            x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2) +
            y1 * (z2 - z3) + y2 * (z3 - z1) + y3 * (z1 - z2) +
            z1 * (x2 - x3) + z2 * (x3 - x1) + z3 * (x1 - x2)
        )

        return triangle_area

    def calulate_w(self):

        
        connectivity = {}
        area = {}
        theta = {}
        laplacien = {}
        curvature = {}

        for (vertex_index, vertex) in enumerate(self.vertices):
            for (face_index, face) in enumerate(self.faces):
                if vertex_index in [face.a, face.b, face.c]:
                    
                    # Connectivity
                    for v in [face.a, face.b, face.c]:
                        connectivity.setdefault(v, [])

                        laplacien.setdefault(vertex_index,0)
                        laplacien[vertex_index] += v - vertex

                        # Add the connectivity to neighboring vertices
                        for neighbor in [face.a, face.b, face.c]:
                            if v != neighbor and neighbor not in connectivity[v]:
                                connectivity[v].append(neighbor)
                    
                    # Area
                    area.setdefault(vertex_index, 0)
                    area[vertex_index] += self.area(face)

                    #Theta
                    if vertex_index == face.a:
                        adj_vertices = [self.vertices[face.b], self.vertices[face.c]]
                    elif vertex_index == face.b:
                        adj_vertices = [self.vertices[face.a], self.vertices[face.c]]
                    elif vertex_index == face.c:
                        adj_vertices = [self.vertices[face.a], self.vertices[face.b]]

                    vector1 = adj_vertices[0] - vertex
                    vector2 = adj_vertices[1] - vertex

                    # Calculez l'angle entre vector1 et vector2
                    angle = np.arccos(np.dot(vector1, vector2) / (np.linalg.norm(vector1) * np.linalg.norm(vector2)))
                    theta.setdefault(vertex_index, [])
                    theta[vertex_index].append(angle)

            # Calcul de la courbure
            curvature.setdefault(vertex_index, None)
            laplacien_pi = laplacien[vertex_index]
            H = - np.linalg.norm(laplacien_pi) / 2
            kg = (2 * np.pi - np.sum(theta[vertex_index])) / area[vertex_index]
            k1 = H + np.sqrt(H**2 - kg)
            k2 = H - np.sqrt(H**2 - kg)
            curvature[vertex_index] = np.abs(k1) + np.abs(k2)
        
        # Calcul de W
        w = []
        for (vertex_index, vertex) in enumerate(self.vertices):
            if len(connectivity[vertex_index]) <= 12 :
                a_neighbors = []
                k_neighbors = []

                for neighbor in connectivity[vertex_index]:
                    a_neighbors.append(area[neighbor])
                    k_neighbors.append(curvature[neighbor])
              
                weight = 0.5 * (area[vertex_index] / np.max(a_neighbors)) + 0.5 * (curvature[vertex_index] / np.max(k_neighbors))
                
                w.append([weight, vertex_index, vertex])

        # Sort the list of weight     
        w = sorted(w, key=lambda x: x[0], reverse=True)

        return w, connectivity
        


    def contract(self, output):
        
        operations=[]
        vertices_restants = []
        
        w, connectivity = self.calulate_w()
        
        for (vertex_index, vertex) in enumerate(self.vertices):
            operations.append(('ev', vertex_index, vertex + 0.25))
        
        
        while w !=[]:
             # Get the vertex with the highest weight
            (weight, vertex_index, vertex) = w.pop(0)
            
            # Iterate through the faces
            for (face_index, face) in enumerate(self.faces):
                # Delete any face related to this vertex
                if face_index not in self.deleted_faces:
                    if vertex_index in [face.a,face.b,face.c]:
                        self.deleted_faces.add(face_index)
                        # Add the instruction to operations stack
                        operations.append(('face', face_index, face))
                        
            #delete vertex that are connected to this vertex from sorted_weight
            # Faites une copie de la liste sorted_weight
            sorted_weight_copy = w.copy()
            for (weight_neighbor, index_neighbor, neighbor) in sorted_weight_copy:
                if index_neighbor in connectivity[vertex_index]:
                    # Supprimez l'élément de la liste d'origine
                    w.remove([weight_neighbor, index_neighbor, neighbor])
                    vertices_restants.append([index_neighbor, neighbor])

            # Delete the vertex
            operations.append(('vertex', vertex_index, vertex))
            
        ######
        # Iterate through the vertex
        for (vertex_index, vertex) in vertices_restants:

            # Iterate through the faces
            for (face_index, face) in enumerate(self.faces):

                # Delete any face related to this vertex
                if face_index not in self.deleted_faces:
                    if vertex_index in [face.a,face.b,face.c]:
                        self.deleted_faces.add(face_index)
                        # Add the instruction to operations stack
                        operations.append(('face', face_index, face))

            # Delete the vertex
            operations.append(('vertex', vertex_index, vertex))
            
                # To rebuild the model, run operations in reverse order
        operations.reverse()
    


        # Write the result in output file
        output_model = obja.Output(output, random_color=True)

        for (ty, index, value) in operations:
            if ty == "vertex":
                output_model.add_vertex(index, value)
            elif ty == "face":
                output_model.add_face(index, value)   
            else:
                output_model.edit_vertex(index, value)
            
            
                
            
        

def main():
    """
    Runs the program on the model given as parameter.
    """
    np.seterr(invalid = 'raise')
    model = Decimater()
    model.parse_file('example/cube.obj')

    with open('example/cube.obja', 'w') as output:
        model.contract(output)


if __name__ == '__main__':
    main()
