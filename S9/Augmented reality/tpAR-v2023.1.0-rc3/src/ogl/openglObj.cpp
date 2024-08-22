/*
 *
 * Demonstrates how to load and display an Wavefront OBJ file. 
 * Using triangles and normals as static object. No texture mapping.
 *
 * OBJ files must be triangulated!!!
 * Non triangulated objects wont work!
 * You can use Blender to triangulate
 *
 */

//#include <windows.h>
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <string.h>

#ifdef __APPLE__
#include <OpenGL/gl.h>
// #include <OpenGL/glu.h>
#include <GLUT/glut.h>
#else
#ifdef _WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
// #include <GL/glu.h>
#include <GL/freeglut.h>
#endif

#include <glm.h>

#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <vector>
#include <cmath>

#define KEY_ESCAPE 27

using namespace std;

/************************************************************************
  Window
 ************************************************************************/

typedef struct
{
    int width;
    int height;
    char* title;

    float field_of_view_angle;
    float z_near;
    float z_far;
} glutWindow;

/*************************************************************************** 
  OBJ Loading 
 ***************************************************************************/

class Model_OBJ
{
public:
    Model_OBJ( );
    float* calculateNormal( float* coord1, float* coord2, float* coord3 );
    int Load( char *filename ); // Loads the model
    void Draw( ); // Draws the model on the screen
    void Release( ); // Release the model

    float* normals; // Stores the normals
    float* Faces_Triangles; // Stores the triangles
    float* vertexBuffer; // Stores the points which make the object
    long TotalConnectedPoints; // Stores the total number of connected verteces
    long TotalConnectedTriangles; // Stores the total number of connected triangles

};


#define POINTS_PER_VERTEX 3
#define TOTAL_FLOATS_IN_TRIANGLE 9
using namespace std;

Model_OBJ::Model_OBJ( )
{
    this->TotalConnectedTriangles = 0;
    this->TotalConnectedPoints = 0;
}

float* Model_OBJ::calculateNormal( float *coord1, float *coord2, float *coord3 )
{
    /* calculate Vector1 and Vector2 */
    float va[3], vb[3], vr[3], val;
    va[0] = coord1[0] - coord2[0];
    va[1] = coord1[1] - coord2[1];
    va[2] = coord1[2] - coord2[2];

    vb[0] = coord1[0] - coord3[0];
    vb[1] = coord1[1] - coord3[1];
    vb[2] = coord1[2] - coord3[2];

    /* cross product */
    vr[0] = va[1] * vb[2] - vb[1] * va[2];
    vr[1] = vb[0] * va[2] - va[0] * vb[2];
    vr[2] = va[0] * vb[1] - vb[0] * va[1];

    /* normalization factor */
    val = sqrt( vr[0] * vr[0] + vr[1] * vr[1] + vr[2] * vr[2] );

    float norm[3];
    norm[0] = vr[0] / val;
    norm[1] = vr[1] / val;
    norm[2] = vr[2] / val;


    return norm;
}

int Model_OBJ::Load( char* filename )
{
    string line;
    ifstream objFile( filename );
    if( objFile.is_open( ) ) // If obj file is open, continue
    {
        objFile.seekg( 0, ios::end ); // Go to end of the file,
        long fileSize = objFile.tellg( ); // get file size
        objFile.seekg( 0, ios::beg ); // we'll use this to register memory for our 3d model

        vertexBuffer = ( float* ) malloc( fileSize ); // Allocate memory for the verteces
        Faces_Triangles = ( float* ) malloc( fileSize * sizeof (float ) ); // Allocate memory for the triangles
        normals = ( float* ) malloc( fileSize * sizeof (float ) ); // Allocate memory for the normals

        int triangle_index = 0; // Set triangle index to zero
        int normal_index = 0; // Set normal index to zero

        while( !objFile.eof( ) ) // Start reading file data
        {
            getline( objFile, line ); // Get line from file

            if( line.c_str( )[0] == 'v' ) // The first character is a v: on this line is a vertex stored.
            {
                line[0] = ' '; // Set first character to 0. This will allow us to use sscanf

                sscanf( line.c_str( ), "%f %f %f ", // Read floats from the line: v X Y Z
                        &vertexBuffer[TotalConnectedPoints],
                        &vertexBuffer[TotalConnectedPoints + 1],
                        &vertexBuffer[TotalConnectedPoints + 2] );
                TotalConnectedPoints += POINTS_PER_VERTEX; // Add 3 to the total connected points

            }
            else if( line.c_str( )[0] == 'f' ) // The first character is an 'f': on this line is a point stored
            {
                line[0] = ' '; // Set first character to 0. This will allow us to use sscanf

                int vertexNumber[4] = {0, 0, 0};
                if( ( line.find( "//" ) == std::string::npos ) && ( line.find( "/" ) == std::string::npos ) )
                {
                    //                    cout << "reading from a b c" << endl;
                    sscanf( line.c_str( ), "%i%i%i", // Read integers from the line:  f 1 2 3
                            &vertexNumber[0], // First point of our triangle. This is an
                            &vertexNumber[1], // pointer to our vertexBuffer list
                            &vertexNumber[2] ); // each point represents an X,Y,Z.
                }
                else if( line.find( "//" ) != std::string::npos )
                {
                    //                    cout << "reading from //" << endl;
                    int a, b, c;
                    sscanf( line.c_str( ), "%i//%i %i//%i %i//%i", // Read integers from the line:  f 1 2 3
                            &vertexNumber[0], &a, // First point of our triangle. This is an
                            &vertexNumber[1], &b, // pointer to our vertexBuffer list
                            &vertexNumber[2], &c ); // each point represents an X,Y,Z.
                    //                    cout << vertexNumber[0] << " " << a << " " << vertexNumber[1] << " " << b << " " << vertexNumber[2] << " " << c << " " << endl;
                }

                vertexNumber[0] -= 1; // OBJ file starts counting from 1
                vertexNumber[1] -= 1; // OBJ file starts counting from 1
                vertexNumber[2] -= 1; // OBJ file starts counting from 1


                /********************************************************************
                 * Create triangles (f 1 2 3) from points: (v X Y Z) (v X Y Z) (v X Y Z).
                 * The vertexBuffer contains all verteces
                 * The triangles will be created using the verteces we read previously
                 */

                int tCounter = 0;
                for( int i = 0; i < POINTS_PER_VERTEX; i++ )
                {
                    Faces_Triangles[triangle_index + tCounter ] = vertexBuffer[3 * vertexNumber[i] ];
                    Faces_Triangles[triangle_index + tCounter + 1 ] = vertexBuffer[3 * vertexNumber[i] + 1 ];
                    Faces_Triangles[triangle_index + tCounter + 2 ] = vertexBuffer[3 * vertexNumber[i] + 2 ];
                    tCounter += POINTS_PER_VERTEX;
                }

                /*********************************************************************
                 * Calculate all normals, used for lighting
                 */
                float coord1[3] = {Faces_Triangles[triangle_index], Faces_Triangles[triangle_index + 1], Faces_Triangles[triangle_index + 2]};
                float coord2[3] = {Faces_Triangles[triangle_index + 3], Faces_Triangles[triangle_index + 4], Faces_Triangles[triangle_index + 5]};
                float coord3[3] = {Faces_Triangles[triangle_index + 6], Faces_Triangles[triangle_index + 7], Faces_Triangles[triangle_index + 8]};
                float *norm = this->calculateNormal( coord1, coord2, coord3 );

                tCounter = 0;
                for( int i = 0; i < POINTS_PER_VERTEX; i++ )
                {
                    normals[normal_index + tCounter ] = norm[0];
                    normals[normal_index + tCounter + 1] = norm[1];
                    normals[normal_index + tCounter + 2] = norm[2];
                    tCounter += POINTS_PER_VERTEX;
                }

                triangle_index += TOTAL_FLOATS_IN_TRIANGLE;
                normal_index += TOTAL_FLOATS_IN_TRIANGLE;
                TotalConnectedTriangles += TOTAL_FLOATS_IN_TRIANGLE;
            }

        }
        cout << "TotalConnectedTriangles: " << TotalConnectedTriangles << endl;
        objFile.close( ); // Close OBJ file
    }
    else
    {
        cout << "Unable to open file";
    }
    return 0;
}

void Model_OBJ::Release( )
{
    free( this->Faces_Triangles );
    free( this->normals );
    free( this->vertexBuffer );
}

void Model_OBJ::Draw( )
{
    glEnableClientState( GL_VERTEX_ARRAY ); // Enable vertex arrays
    glEnableClientState( GL_NORMAL_ARRAY ); // Enable normal arrays
    glVertexPointer( 3, GL_FLOAT, 0, Faces_Triangles ); // Vertex Pointer to triangle array
    glNormalPointer( GL_FLOAT, 0, normals ); // Normal pointer to normal array
    glDrawArrays( GL_TRIANGLES, 0, TotalConnectedTriangles ); // Draw the triangles
    glDisableClientState( GL_VERTEX_ARRAY ); // Disable vertex arrays
    glDisableClientState( GL_NORMAL_ARRAY ); // Disable normal arrays
}

/***************************************************************************
 * Program code
 ***************************************************************************/

Model_OBJ obj;
GLMmodel *pmodel = NULL; /* the loaded model */
GLfloat modelDim[3];
float g_rotation;
glutWindow win;

GLuint mode = 0;
int wireframe = 0; /* Draw modes */
int show_axis = 1;
int smooth = 1;
int material = 1;
int textured = 0;
int two_sided = 1;
int lighting = 1;

void DrawModel( void )
{

    mode = GLM_NONE; /* reset mode */

    if( smooth )
        mode = mode | GLM_SMOOTH;
    else
        mode = mode | GLM_FLAT;

    if( two_sided )
        mode = mode | GLM_2_SIDED;

    if( material )
        mode = mode | GLM_MATERIAL;
    else
        mode = mode | GLM_COLOR;

    if( textured && material )
        mode = mode | GLM_TEXTURE;

    glPushMatrix( );
    if( pmodel )
        glmDraw( pmodel, mode );
    glPopMatrix( );
}

void DrawAxis( float scale )
{
    glPushMatrix( );
    glDisable( GL_LIGHTING );
    glDisable( GL_TEXTURE_2D );
    glScalef( scale, scale, scale );

    glBegin( GL_LINES );

    glColor3f( 1.0, 0.0, 0.0 );
    glVertex3f( .8f, 0.05f, 0.0 );
    glVertex3f( 1.0, 0.25f, 0.0 ); /*  Letter X */
    glVertex3f( 0.8f, .25f, 0.0 );
    glVertex3f( 1.0, 0.05f, 0.0 );
    glVertex3f( 0.0, 0.0, 0.0 );
    glVertex3f( 1.0, 0.0, 0.0 ); /*  X axis */

    glColor3f( 0.0, 1.0, 0.0 );
    glVertex3f( 0.0, 0.0, 0.0 );
    glVertex3f( 0.0, 1.0, 0.0 ); /*  Y axis */

    glColor3f( 0.0, 0.0, 1.0 );
    glVertex3f( 0.0, 0.0, 0.0 );
    glVertex3f( 0.0, 0.0, 1.0 ); /*  Z axis */
    glEnd( );
    if( lighting )
        glEnable( GL_LIGHTING );
    if( lighting )
        glEnable( GL_TEXTURE_2D );
    glColor3f( 1.0, 1.0, 1.0 );
    glPopMatrix( );
}

static void draw_axis( float scale )
{
    glPushMatrix( );
    glDisable( GL_LIGHTING );

    glScalef( scale, scale, scale );

    glBegin( GL_LINES );

    glColor3f( 1.0, 0.0, 0.0 );
    glVertex3f( .8f, 0.05f, 0.0 );
    glVertex3f( 1.0, 0.25f, 0.0 ); // Letter X
    glVertex3f( 0.8f, .25f, 0.0 );
    glVertex3f( 1.0, 0.05f, 0.0 );
    glVertex3f( 0.0, 0.0, 0.0 );
    glVertex3f( 1.0, 0.0, 0.0 ); // X axis

    glColor3f( 0.0, 1.0, 0.0 );
    glVertex3f( 0.0, 0.0, 0.0 );
    glVertex3f( 0.0, 1.0, 0.0 ); // Y axis

    glColor3f( 0.0, 0.0, 1.0 );
    glVertex3f( 0.0, 0.0, 0.0 );
    glVertex3f( 0.0, 0.0, 1.0 ); // Z axis
    glEnd( );
    glEnable( GL_LIGHTING );
    glColor3f( 1.0, 1.0, 1.0 );
    glPopMatrix( );

}

void display( )
{
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    glLoadIdentity( );
    //    gluLookAt( 0, 1, 4, 0, 0, 0, 0, 1, 0 );
    glPushMatrix( );
    GLfloat gLightPos[] = {100.0, 200.0, 200.0, 0.0};
    glEnable( GL_LIGHTING );
    glLightfv( GL_LIGHT0, GL_POSITION, gLightPos );

    glTranslatef( 0, 0, -1.95 );
    glRotatef( g_rotation, 0, 1, 0 );
    //    glRotatef( 90, 0, 1, 0 );
    g_rotation += 0.1;
    //    draw_axis( 1.0f );
    //    glScalef( 0.025, 0.025, 0.025 );


    if( show_axis )
        DrawAxis( 1.0f );
    if( wireframe ) /* if Wireframe is checked */
        glPolygonMode( GL_FRONT_AND_BACK, GL_LINE ); /* draw wireframe */
    else /* else */
        glPolygonMode( GL_FRONT_AND_BACK, GL_FILL ); /* draw filled polygons */

    //    glScalef( 0.25, 0.25, 0.25);
    glTranslatef( 0, 0.5 * 0.25 * modelDim[1], 0 );
    DrawModel( );

    //    glutSolidTeapot( 15 );
    //    obj.Draw( );

    glPopMatrix( );
    glutSwapBuffers( );
}

void initialize( )
{
    glMatrixMode( GL_PROJECTION );
    glViewport( 0, 0, win.width, win.height );
    GLfloat aspect = ( GLfloat ) win.width / win.height;
    glMatrixMode( GL_PROJECTION );
    glLoadIdentity( );
    gluPerspective( win.field_of_view_angle, aspect, win.z_near, win.z_far );
    glMatrixMode( GL_MODELVIEW );
    glShadeModel( GL_SMOOTH );
    glClearColor( 0.1f, 0.1f, 0.1f, 0.5f );
    glClearDepth( 1.0f );
    glEnable( GL_DEPTH_TEST );
    glDepthFunc( GL_LEQUAL );
    glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );

    GLfloat amb_light[] = {0.9, 0.9, 0.9, 1.0};
    GLfloat diffuse[] = {0.6, 0.6, 0.6, 1};
    GLfloat specular[] = {0.7, 0.7, 0.3, 1};
    GLfloat gLightPos[] = {100.0, 200.0, -200.0, 0.0};
    glLightModelfv( GL_LIGHT_MODEL_AMBIENT, amb_light );
    glLightfv( GL_LIGHT0, GL_DIFFUSE, diffuse );
    glLightfv( GL_LIGHT0, GL_SPECULAR, specular );
    glLightfv( GL_LIGHT0, GL_POSITION, gLightPos );

    GLfloat mat_ambient[] = {0.7, 0.7, 0.7, 1.0};
    GLfloat mat_diffuse[] = {0.8, 0.8, 0.8, 1.0};
    GLfloat mat_specular[] = {1.0, 1.0, 1.0, 1.0};
    GLfloat high_shininess[] = {100.0};
    glMaterialfv( GL_FRONT, GL_AMBIENT, mat_ambient );
    glMaterialfv( GL_FRONT, GL_DIFFUSE, mat_diffuse );
    glMaterialfv( GL_FRONT, GL_SPECULAR, mat_specular );
    glMaterialfv( GL_FRONT, GL_SHININESS, high_shininess );

    glEnable( GL_LIGHT0 );
    glEnable( GL_COLOR_MATERIAL );
    glShadeModel( GL_SMOOTH );
    glLightModeli( GL_LIGHT_MODEL_TWO_SIDE, GL_FALSE );
    glDepthFunc( GL_LEQUAL );
    glEnable( GL_DEPTH_TEST );
    glEnable( GL_LIGHTING );
    glEnable( GL_LIGHT0 );
}

void keyboard( unsigned char key, int x, int y )
{
    switch( key )
    {
        case 'w':
        case 'W':
        {
            wireframe = !wireframe;
            break;
        }
        case 'd':
        case 'D':
        {
            two_sided = !two_sided;
            break;
        }
        case 's':
        case 'S':
        {
            smooth = !smooth;
            break;
        }
        case 'm':
        case 'M':
        {
            material = !material;
            if( !material && textured )
                textured = 0;
            break;
        }
        case 't':
        case 'T':
        {
            textured = !textured;
            if( !material && textured )
                material = 1;
            break;
        }
        case KEY_ESCAPE:
            exit( 0 );
            break;
        default:
            break;
    }
}

int main( int argc, char **argv )
{
    // set window values
    win.width = 640;
    win.height = 480;
    win.title = "OpenGL/GLUT OBJ Loader.";
    win.field_of_view_angle = 45;
    win.z_near = 1.0f;
    win.z_far = 500.0f;

    // initialize and run program
    glutInit( &argc, argv ); // GLUT initialization
    glutInitDisplayMode( GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH ); // Display Mode
    glutInitWindowSize( win.width, win.height ); // set window size
    glutCreateWindow( win.title ); // create Window
    glutDisplayFunc( display ); // register Display Function
    glutIdleFunc( display ); // register Idle Function
    glutKeyboardFunc( keyboard ); // register Keyboard Handler
    initialize( );

    //    load_obj(argv[1], suzanne_vertices, suzanne_normals, suzanne_elements);
    obj.Load( argv[1] );
    if( !pmodel )
    { /* load up the model */
        pmodel = glmReadOBJ( argv[1] );
        if( !pmodel )
        {
            printf( "\nUsage: objviewV2 <-s> <obj filename>\n" );
            return EXIT_FAILURE;
        }
        glmUnitize( pmodel );
        glmDimensions( pmodel, modelDim );
        cerr << "Model dimensions WxHxD: " << modelDim[0] << " x " << modelDim[1] << " x " << modelDim[2] << endl;

        glmScale( pmodel, 0.25 );

        glmVertexNormals( pmodel, 90.0, GL_TRUE );
    }
    glutMainLoop( ); // run GLUT mainloop
    return 0;
}
