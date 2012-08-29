#include <SDL/SDL.h>

int main(int argc, char ** argv) {
  SDL_Rect** modes;
  const SDL_VideoInfo* info;

  int i;

  if ( SDL_Init(SDL_INIT_VIDEO) < 0 ) {
    fprintf(stderr, "Unable to init SDL: %s\n", SDL_GetError());
    exit(1);
  }
  
  /* Get available fullscreen/hardware modes */
  modes = SDL_ListModes(NULL, SDL_FULLSCREEN|SDL_OPENGL);
   
  /* Check if there are any modes available */
  if (modes == (SDL_Rect**)0) {
    printf("No modes available!\n");
    exit(-1);
  }
  
  /* Check if our resolution is restricted */
  if (modes == (SDL_Rect**)-1) {
    printf("All resolutions available.\n");
  } else{
    /* Print valid modes */
    printf("Available Modes\n");
    for (i=0; modes[i]; ++i) {
      printf("  %d x %d\n", modes[i]->w, modes[i]->h);
    }
  }

  info = SDL_GetVideoInfo();
  printf(" suggested %d bits per pixel\n", info->vfmt->BitsPerPixel);
  return 0;
}
