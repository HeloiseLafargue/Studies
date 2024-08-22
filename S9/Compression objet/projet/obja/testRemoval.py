#!/usr/bin/env python

import obja
import numpy as np
import sys

def calculate_face_normal(vertex1, vertex2, vertex3):
    """
    Calculate the normal vector of a triangle defined by its vertices.
    """
    edge1 = vertex2 - vertex1
    edge2 = vertex3 - vertex1
    normal = np.cross(edge1, edge2)
    return normal / np.linalg.norm(normal)
# compute the 

class Decimater(obja.Model):
    """
    A simple class that decimates a 3D model stupidly.
    """
    def __init__(self):
        super().__init__()
        self.deleted_faces = set()

    def get_vertex_connectivity(self):
        """
        Get the vertex connectivity of the model.
        """
        vertex_connectivity = {}
        vertex_area = {}

        for (vertex_index, vertex) in enumerate(self.vertices):
            # Initialize the list of connectivity for this vertex
            vertex_connectivity[vertex_index] = []

        for (face_index, face) in enumerate(self.faces):
            for v in [face.a, face.b, face.c]:
                # Add the connectivity to neighboring vertices
                for neighbor in [face.a, face.b, face.c]:
                    if v != neighbor and neighbor not in vertex_connectivity[v]:
                        vertex_connectivity[v].append(neighbor)

        return vertex_connectivity
    
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

    
    def calculate_vertex_area(self, vertex_index):
        """
        Calculate the area around a vertex specified by its index.
        """
        vertex = self.vertices[vertex_index]
        connected_faces = []

        # Find the faces connected to the vertex
        for (face_index, face) in enumerate(self.faces):
            if vertex_index in [face.a, face.b, face.c]:
                connected_faces.append(face)

        # Calculate the area by summing the areas of connected faces
        vertex_area = 0.0
        for face in connected_faces:
            # Assuming 'area' is a function that calculates the area of a triangle
            triangle_area = self.area(face)
            vertex_area += triangle_area

        return vertex_area
    

    def estimate_vertex_curvature(self, vertex_index, neighbors):
        """
        Estimate the curvature of a vertex specified by its index.
        'vertex_index' is the index of the vertex.
        """
        vertex = self.vertices[vertex_index]
        
        # Calculate the normal vector of the vertex by averaging the normals of neighboring faces
        normal_sum = np.zeros(3)
        for neighbor in neighbors:
            # Calculate the normal of the face formed by the vertex and its two neighbors
            face_normal = calculate_face_normal(vertex, self.vertices[neighbor[0]], self.vertices[neighbor[1]])
            normal_sum += face_normal
        
        vertex_normal = normal_sum / len(neighbors)
        
        # Calculate the curvature using the dot product of the vertex normal and its unit normal
        curvature = 1.0 - np.linalg.norm(vertex_normal)
        
        return curvature


    def contract(self, output):
        """
        Decimates the model stupidly, and write the resulting obja in output.
        """
        operations = []

        vertex_connectivity = {}
        vertex_area = {}
        vertex_curvature = {}
        theta_you_know = {}
        laplacien_you_know = {}
        curvature= {}
        for (vertex_index, vertex) in enumerate(self.vertices):

            # Initialize the list of connectivity for this vertex
            vertex_connectivity[vertex_index] = []
            vertex_area[vertex_index] = 0.0
            vertex_curvature[vertex_index] = 0.0
           

        for (vertex_index, vertex) in enumerate(self.vertices):

            normal_vector_to_the_vertex= np.zeros(3)
            num_triangles= 0
            
            for (face_index, face) in enumerate(self.faces):
                print(face)            

                if vertex_index in [face.a, face.b, face.c]:
                    vertex_a = self.vertices[face.a]
                    vertex_b = self.vertices[face.b]
                    vertex_c = self.vertices[face.c]

                    # Determine the neighboring vertices which are not vertex
                    if vertex_index == face.a:
                        adj_vertices = [vertex_b, vertex_c]
                    elif vertex_index == face.b:
                        adj_vertices = [vertex_a, vertex_c]
                    elif vertex_index == face.c:
                        adj_vertices = [vertex_a, vertex_b]

                    if len(adj_vertices) == 2:
                        vector1 = adj_vertices[0] - vertex
                        vector2 = adj_vertices[1] - vertex

                        # Calculez l'angle entre vector1 et vector2
                    angle = np.arccos(np.dot(vector1, vector2) / (np.linalg.norm(vector1) * np.linalg.norm(vector2)))
                    
                        # Ajoutez l'angle à la liste correspondante dans le dictionnaire
                    theta_you_know.setdefault(vertex_index, []).append(angle)
                                                
                    #Connectivity
                    for v in [face.a, face.b, face.c]:
                        # Add the connectivity to neighboring vertices

                        if v != vertex_index :
                                laplacien_you_know.setdefault(vertex_index,0)
                                laplacien_you_know[vertex_index] += v - vertex
                        for neighbor in [face.a, face.b, face.c]:
                            if v != neighbor and neighbor not in vertex_connectivity[v]:

                                vertex_connectivity[v].append(neighbor)

                #Area
                if vertex_index in [face.a, face.b, face.c]:
                    vertex_area[vertex_index] += (self.area(face))
            curvature.setdefault(vertex_index, None)
            # Calcul de la courbure dans tous les cas
            try:
                laplacien_pi = laplacien_you_know[vertex_index]
                H = - np.linalg.norm(laplacien_pi) / 2
                kg = (2 * np.pi - np.sum(theta_you_know[vertex_index])) / vertex_area[vertex_index]
                k1 = H + np.sqrt(H**2 - kg)
                k2 = H - np.sqrt(H**2 - kg)
                curvature[vertex_index] = np.abs(k1) + np.abs(k2)
            except KeyError:
                curvature[vertex_index] = None  # Vous pouvez définir une valeur par défaut, par exemple None, ou faire autre chose

           
           
                    



            # vertex_normal_vector[vertex_index] = normal_vector_to_the_vertex / num_triangles

                

        file_weight = []
        for (vertex_index, vertex) in enumerate(self.vertices):
            # Weight
            if len(vertex_connectivity[vertex_index]) <= 12 :
                a_neighbors = []
                k_neighbors = []

                for neighbor in vertex_connectivity[vertex_index]:
                    a_neighbors.append(vertex_area[neighbor])
                    k_neighbors.append(curvature[neighbor])
                try :
                    w = 0.5 * (vertex_area[vertex_index] / np.max(a_neighbors)) + 0.5 * (curvature[vertex_index] / np.max(k_neighbors))
                except ValueError:
                    w=0
                # print(curvature[vertex_index])
                #weight = 0.5 * (vertex_area[vertex_index]/np.max(vertex_area[connectivity])) + 0.5 * (vertex_curvature[vertex_index]/np.max(vertex_curvature[vertex_connectivity[vertex_index]]))
                file_weight.append([w, vertex_index, vertex])

        # Sort the list of weight
        print(file_weight)
        
        sorted_weight = sorted(file_weight, key=lambda x: x[0], reverse=True)

        
        # # Iterate through the vertex
        # for (vertex_index, vertex) in enumerate(self.vertices):
        #     # Iterate through the faces
        #     for (face_index, face) in enumerate(self.faces):

        #         # Delete any face related to this vertex
        #         if face_index not in self.deleted_faces:
        #             if vertex_index in [face.a,face.b,face.c]:
        #                 self.deleted_faces.add(face_index)
        #                 # Add the instruction to operations stack
        #                 operations.append(('face', face_index, face))
        #  # Delete the vertex
        #     operations.append(('vertex', vertex_index, vertex))
        
        while sorted_weight!=[]:
            # Get the vertex with the highest weight
            (w, vertex_index, vertex) = sorted_weight.pop(0)

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
            sorted_weight_copy = sorted_weight.copy()
            for (weight_neighbor, index_neighbor, neighbor) in sorted_weight_copy:
                if index_neighbor in vertex_connectivity[vertex_index]:
                    # Supprimez l'élément de la liste d'origine
                    sorted_weight.remove([weight_neighbor, index_neighbor, neighbor])

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
                
                try:
                   
                    output_model.add_face(index, value)  
                    print("youpi")
                except:
                    print("erreur")

                    
            else:
                output_model.edit_vertex(index, value)

def main():
    """
    Runs the program on the model given as parameter.
    """
    np.seterr(invalid = 'raise')
    model = Decimater()
    model.parse_file('example/suzanne.obj')

    with open('example/suzanne.obja', 'w') as output:
        model.contract(output)


if __name__ == '__main__':
    main()
