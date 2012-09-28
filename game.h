#ifndef GAME_H
#define GAME_H

struct InputState_;

void game_init();
void game_step(long delta, struct InputState_* state);
void game_shutdown();

#endif
