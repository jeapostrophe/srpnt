#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <portaudio.h>

#define NUM_SECONDS (5)
#define SAMPLE_RATE (44100)
#define FRAMES_PER_BUFFER (SAMPLE_RATE/60)
#define FREQ (261.63)

void maybeDie( PaError err ) {
  if ( err == paNoError ) { return; }
  Pa_Terminate();
  fprintf( stderr, "An error occured while using the portaudio stream\n" );
  fprintf( stderr, "Error number: %d\n", err );
  fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
  exit(err); }

int main(void) {
  float buf[FRAMES_PER_BUFFER];
  
  printf("PortAudio Test: output sine wave. SR = %d, BufSize = %d\n", SAMPLE_RATE, FRAMES_PER_BUFFER);

  maybeDie( Pa_Initialize() );

  PaStreamParameters outputParameters;
  outputParameters.device = Pa_GetDefaultOutputDevice(); /* default output device */
  if (outputParameters.device == paNoDevice) {
    fprintf(stderr,"Error: No default output device.\n");
    exit(1); }
  outputParameters.channelCount = 1;
  outputParameters.sampleFormat = paFloat32;
  outputParameters.suggestedLatency = Pa_GetDeviceInfo( outputParameters.device )->defaultLowOutputLatency;
  outputParameters.hostApiSpecificStreamInfo = NULL;

  PaStream *stream;
  maybeDie( Pa_OpenStream(&stream, NULL, &outputParameters, SAMPLE_RATE, FRAMES_PER_BUFFER, paClipOff, NULL, NULL ) );
  maybeDie( Pa_StartStream( stream ) );

  printf("Play for %d seconds.\n", NUM_SECONDS );
  float cycle_per = 0.0;
  for ( int i = 0; i < NUM_SECONDS*60; i++ ) {
    for ( int j = 0; j < FRAMES_PER_BUFFER; j++ ) {
      float step_per = (float)FREQ / (float)SAMPLE_RATE;
      float next_per = cycle_per + step_per;
      cycle_per = next_per - floorf( next_per );
      buf[j] = (float) sin( cycle_per * M_PI * 2.0 ); }
    maybeDie( Pa_WriteStream( stream, buf, FRAMES_PER_BUFFER ) ); }
  
  maybeDie( Pa_StopStream( stream ) );
  maybeDie( Pa_CloseStream( stream ) );
  Pa_Terminate();
  
  return 0; }
