
#include "GameObjectGenerator.h"
#include "Game.h"
#include "Road.h"
#include "Wall.h"
#include "Goal.h"
#include "Coin.h"
#include "Obstacle.h"
#include "Barrera.h"
#include "Pedestrian.h"
#include "Crosswalk.h"
#include "CraneW.h"
#include "CraneL.h"
#include "Hook.h"
#include "Dirt.h"
#include "Oil.h"
#include "Well.h"
#include "Boost.h"
#include "CrazyArch.h"
#include "Bombardier.h"
#include "Bomb.h"
#include "ColliderLaps.h"

GameObjectGenerator::GameObjectGenerator(Game *game): game(game){}

void GameObjectGenerator::generateWorld(){
    int W = game->ROAD_WIDTH;
    int L = game->ROAD_LENGTH;
    
    // Camino
    glm::vec3 roadPos(0, -50.1, L/2 - 1000);
    auto road = new Road(game, roadPos, glm::vec3(W, 0, L));
    
    game->addGameObject(road);


	// Camino2
	glm::vec3 roadPos2(2500, -50.1, L/2 - 1000);
	auto road2 = new Road(game, roadPos2, glm::vec3(W, 0, L));


	game->addGameObject(road2);

	// Camino3
	glm::vec3 roadPos3(1250, -50.1, -L/4 + 300); // Este z es un calculo a ojo
	auto road3 = new Road(game, roadPos3, glm::vec3(4500, 0, L/4));

	game->addGameObject(road3);

	// Camino4
	glm::vec3 roadPos4(1250, -50.1, L + 250);
	auto road4 = new Road(game, roadPos4, glm::vec3(4500, 0, L / 4));

	game->addGameObject(road4);

	road->lightEnable1();
	road->lightEnable2();
	//road->lightEnable3();
	//road->lightEnable4();
	road->lightEnable5();


//
    int wallSize = 100;

	/*
    ofImage circuito;
    circuito.load("circuito.png");

    int w = circuito.getWidth()/16;
    int h = circuito.getHeight()/16;
    circuito.resize(w, h);
	*/


//    for(int x = 0; x < w; x++){
//        for(int y = 0; y < h; y++){
//            if(circuito.getColor(x, y).a > 250){
//
//                auto wall = new Wall(game,
//                                glm::vec3(x*wallSize - 5000, roadPos.y, y*wallSize -2000),
//                                glm::vec3(wallSize*0.99));
//                game->addGameObject(wall);
//            }
//        }
//    }

    
//     WALL with parts
//    for(int l = 0; l < L; l += wallSize){
//        auto wall_r = new Wall(game,
//                        glm::vec3(-W/2, roadPos.y, l - 1000 + wallSize/2),
//                        glm::vec3(wallSize));
//
//        wall_r->isFixed = true;
//        game->addGameObject(wall_r);
//
//    }
    
    // Paredes Camino 1
    auto wall_r = new Wall(game,
                    glm::vec3(-W/2, roadPos.y, roadPos.z),
                           glm::vec3(wallSize, wallSize, L));
    game->addGameObject(wall_r);

    

    auto wall_l = new Wall(game,
                    glm::vec3(W/2, roadPos.y, roadPos.z),
                           glm::vec3(wallSize, wallSize, L));
    game->addGameObject(wall_l);

	// Paredes Camino 2
	auto wall2_r = new Wall(game,
		glm::vec3(roadPos2.x - W/2, roadPos.y, roadPos.z),
		glm::vec3(wallSize, wallSize, L));
	game->addGameObject(wall2_r);



	auto wall2_l = new Wall(game,
		glm::vec3(roadPos2.x + W/2, roadPos.y, roadPos.z),
		glm::vec3(wallSize, wallSize, L));
	game->addGameObject(wall2_l);

	// Paredes Camino 3
	auto wall3_r = new Wall(game,
		glm::vec3(roadPos3.x - 4500 / 2, roadPos.y, roadPos3.z),
		glm::vec3(wallSize, wallSize, L / 4));
	game->addGameObject(wall3_r);

	auto wall3_l = new Wall(game,
		glm::vec3(roadPos3.x + 4500 / 2, roadPos.y, roadPos3.z),
		glm::vec3(wallSize, wallSize, L / 4));
	game->addGameObject(wall3_l);

	auto wall3_f = new Wall(game,
		glm::vec3(roadPos3.x, roadPos.y, roadPos3.z + L/8),
		glm::vec3(500, wallSize, wallSize));
	game->addGameObject(wall3_f);

	auto wall3_b = new Wall(game,
		glm::vec3(roadPos3.x, roadPos.y, roadPos3.z - L/8),
		glm::vec3(4500, wallSize, wallSize));
	game->addGameObject(wall3_b);

	// Paredes Camino 3
	auto wall4_r = new Wall(game,
		glm::vec3(roadPos4.x - 4500 / 2, roadPos.y, roadPos4.z),
		glm::vec3(wallSize, wallSize, L / 4));
	game->addGameObject(wall4_r);

	auto wall4_l = new Wall(game,
		glm::vec3(roadPos4.x + 4500 / 2, roadPos.y, roadPos4.z),
		glm::vec3(wallSize, wallSize, L / 4));
	game->addGameObject(wall4_l);
	
	auto wall4_f = new Wall(game,
		glm::vec3(roadPos4.x, roadPos.y, roadPos4.z - L / 8),
		glm::vec3(500, wallSize, wallSize));
	game->addGameObject(wall4_f);

	auto wall4_b = new Wall(game,
		glm::vec3(roadPos4.x, roadPos.y, roadPos4.z + L / 8),
		glm::vec3(4500, wallSize, wallSize));
	game->addGameObject(wall4_b);
	
    
	//TUNEL
	auto tunwall_r = new Wall(game,
		glm::vec3(roadPos2.x - W / 3, roadPos.y, roadPos.z),
		glm::vec3(wallSize*6, wallSize*8, L/3));
	game->addGameObject(tunwall_r);

	auto tunwall_l = new Wall(game,
		glm::vec3(roadPos2.x + W / 3, roadPos.y, roadPos.z),
		glm::vec3(wallSize*6, wallSize*8, L/3));
	game->addGameObject(tunwall_l);

	auto tunwall_t = new Wall(game,
		glm::vec3(roadPos2.x, roadPos.y + 800, roadPos.z),
		glm::vec3(W, wallSize * 8, L/3));
	game->addGameObject(tunwall_t);
    

    // Goal
    auto goal = new Goal(game,
                    glm::vec3(0, roadPos.y, roadPos.z + L/2),
                           glm::vec3(W, 100, 100));
    goal->isFixed = true;
    game->addGameObject(goal);
    
    
    // Para camino 1
    // Coins
    for (int i=0; i < 2; i++){
        auto coin = new Coin(game,
            glm::vec3(roadPos.x + rand()%(W-100) - (W/2 - 50), roadPos.y + 25, roadPos.z + rand()%(L - 100) - (L/2 - 50)), glm::vec3(50));
        
        game->addGameObject(coin);
    }
    
     // Obstaculos
    for (int i=0; i < 2; i++){
        auto obstacle = new Obstacle(game,
            glm::vec3(roadPos.x + rand()%(W-100) - (W/2 - 50), roadPos.y + 50, roadPos.z + rand()%(L - 100) - (L/2 - 50)), glm::vec3(100));
        
        game->addGameObject(obstacle);
    }
    
    // Boost
    for (int i=0; i < 1; i++){
        auto boost = new Boost(game, glm::vec3(roadPos.x + rand()%(W-100) - (W/2 - 50), roadPos.y, roadPos.z + rand()%(L - 100) - (L/2 - 50)), glm::vec3(100, 30, 100));
        
        game->addGameObject(boost);
    }
    
       
    // Para camino 3
    // Coins
    for (int i=0; i < 2; i++){
        auto coin = new Coin(game,
            glm::vec3(roadPos3.x + rand()%4400 - 2200, roadPos.y + 25, roadPos3.z + rand()%(L/4 - 100) - (L/8 - 50)), glm::vec3(50));
        
        game->addGameObject(coin);
    }
    
    // Obstaculos
   for (int i=0; i < 2; i++){
       auto obstacle = new Obstacle(game,
           glm::vec3(roadPos3.x + rand()%4400 - 2200, roadPos.y + 50, roadPos3.z + rand()%(L/4 - 100) - (L/8 - 50)), glm::vec3(100));
       
       game->addGameObject(obstacle);
    }
    
    // Boost
    for (int i=0; i < 1; i++){
        auto boost = new Boost(game, glm::vec3(roadPos3.x + rand()%4400 - 2200, roadPos.y, roadPos3.z + rand()%(L/4 - 100) - (L/8 - 50)), glm::vec3(100, 30, 100));
        
        game->addGameObject(boost);
    }
    
    // Para camino 4
    // Coins
    for (int i=0; i < 2; i++){
        auto coin = new Coin(game,
            glm::vec3(roadPos4.x + rand()%4400 - 2200, roadPos.y + 25, roadPos4.z + rand()%(L/4 - 100) - (L/8 - 50)), glm::vec3(50));
        
        game->addGameObject(coin);
    }
    
    // Obstaculos
   for (int i=0; i < 2; i++){
       auto obstacle = new Obstacle(game,
           glm::vec3(roadPos4.x + rand()%4400 - 2200, roadPos.y + 50, roadPos4.z + rand()%(L/4 - 100) - (L/8 - 50)), glm::vec3(100));
       
       game->addGameObject(obstacle);
    }
    
    // Boost
    for (int i=0; i < 1; i++){
        auto boost = new Boost(game, glm::vec3(roadPos4.x + rand()%4400 - 2200, roadPos.y, roadPos4.z + rand()%(L/4 - 100) - (L/8 - 50)), glm::vec3(100, 30, 100));
        
        game->addGameObject(boost);
    }

    
    
    
    // Barrera
    auto wall_c = new Wall(game,
                    glm::vec3(0, roadPos.y + 600, roadPos.z),
                           glm::vec3(W, wallSize, wallSize));
    wall_c -> setColor(ofColor::black);
    game->addGameObject(wall_c);
    
    auto barrera = new Barrera(game,
                    glm::vec3(0, roadPos.y + 500, roadPos.z),
                           glm::vec3(W - 200, wallSize, wallSize));
    game->addGameObject(barrera);
    
    
    
	auto pasopeaton = new Crosswalk(game, glm::vec3(0, -49, 500), glm::vec3(W, 0, 200));
	game->addGameObject(pasopeaton);
    
    //Peatón
    auto pedestrian = new Pedestrian(game,
        glm::vec3(W/2 - 100, -25, 500), glm::vec3(50, 150, 50));

    game->addGameObject(pedestrian);
    
    
    
    // Grúa
    auto Crane_h = new Wall(game,
                    glm::vec3(roadPos.x, roadPos.y, 2000),
                           glm::vec3(wallSize,  W, wallSize));
    game->addGameObject(Crane_h);

    auto Crane_w = new CraneW(game, glm::vec3(roadPos.x, roadPos.y+1000, 2000), glm::vec3(W/2, wallSize, wallSize));
    
    game->addGameObject(Crane_w);
	
    
    auto Crane_l = new CraneL(game, glm::vec3(roadPos.x, roadPos.y, 2000), glm::vec3(2, W, 2));
    game->addGameObject(Crane_l);
	
    
    auto Hook = new class Hook(game, glm::vec3(roadPos.x, roadPos.y + 500, 2000), glm::vec3(wallSize, wallSize, wallSize));
    
    game->addGameObject(Hook);
    
    auto Crane_wh = new CraneW(game, glm::vec3(roadPos.x, roadPos.y - 100, 2000), glm::vec3(W/2, wallSize, wallSize));
    
    game->addGameObject(Crane_wh);
    
    

    
	// esto representa el collider, el plano esta en dirt.cpp
	auto tierra = new Dirt(game, glm::vec3(2000, -48, 300), glm::vec3(1000, 30, 1200));
	game->addGameObject(tierra);

	// esto representa el collider, el plano esta en dirt.cpp
	auto aceite = new Oil(game, glm::vec3(500, -48, -1700), glm::vec3(1500, 30, 1500));
	game->addGameObject(aceite);

	// esto representa el collider, el plano esta en dirt.cpp
	auto pozo = new Well(game, glm::vec3(2200, -48, 9500), glm::vec3(800, 30, 800));
	game->addGameObject(pozo);

	// esto representa el collider, el plano esta en dirt.cpp
	auto boost = new Boost(game, glm::vec3(300, -48, 400), glm::vec3(100, 30, 100));
	game->addGameObject(boost);




	// Arco loco
	auto posteArcoDer = new Wall(game,
		glm::vec3(0, -51, 8500),
		glm::vec3(wallSize, 1000, wallSize));
	game->addGameObject(posteArcoDer);

	auto posteArcoIzq= new Wall(game,
		glm::vec3(600, -51, 8500),
		glm::vec3(wallSize, 1000, wallSize));
	game->addGameObject(posteArcoIzq);

	auto panelArco = new CrazyArch(game, glm::vec3(300, -48, 8500), glm::vec3(600, 300, wallSize));
	game->addGameObject(panelArco);
    
    //Bombardero
    auto bombardier = new Bombardier(game, glm::vec3(roadPos3.x, roadPos3.y + 800, roadPos3.z), glm::vec3(100, 100, 100));
    game->addGameObject(bombardier);

    auto bomb = new Bomb(game,  glm::vec3(roadPos3.x, roadPos3.y + 1200, roadPos3.z), glm::vec3(500, 500, 500));
    game->addGameObject(bomb);


    // ColliderLaps
    auto colliderLaps = new ColliderLaps(game, glm::vec3(roadPos2.x, roadPos.y, roadPos.z), glm::vec3(wallSize * 6, wallSize * 8, L / 3));

    game->addGameObject(colliderLaps);
    
    
    
}
