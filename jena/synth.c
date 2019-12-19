#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <portaudio.h>

#define NUM_SECONDS (5)
#define SAMPLE_RATE (44100)
#define FRAMES_PER_BUFFER (SAMPLE_RATE/60)

#define TABLE_SIZE (200)
typedef struct {
  float sine[TABLE_SIZE];
  int left_phase;}
  paTestData;

/* This routine will be called by the PortAudio engine when audio is needed.
** It may called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int patestCallback( const void *inputBuffer, void *outputBuffer,
                           unsigned long framesPerBuffer,
                           const PaStreamCallbackTimeInfo* timeInfo,
                           PaStreamCallbackFlags statusFlags,
                           void *userData ) {
  paTestData *data = (paTestData*)userData;
  float *out = (float*)outputBuffer;

  /* Prevent unused variable warnings. */
  (void) timeInfo;
  (void) statusFlags;
  (void) inputBuffer;

  for( unsigned long i = 0; i < framesPerBuffer; i++ ) {
    *out++ = data->sine[data->left_phase];  /* left */
    data->left_phase += 1;
    if( data->left_phase >= TABLE_SIZE ) data->left_phase -= TABLE_SIZE; }

  return paContinue; }

void maybeDie( PaError err ) {
  if ( err == paNoError ) { return; }
  Pa_Terminate();
  fprintf( stderr, "An error occured while using the portaudio stream\n" );
  fprintf( stderr, "Error number: %d\n", err );
  fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
  exit(err); }

int main(void) {
  printf("PortAudio Test: output sine wave. SR = %d, BufSize = %d\n", SAMPLE_RATE, FRAMES_PER_BUFFER);

  /* initialise sinusoidal wavetable */
  paTestData data;
  for( int i = 0; i < TABLE_SIZE; i++ ) {
    data.sine[i] = (float) sin( ((double)i/(double)TABLE_SIZE) * M_PI * 2. ); }
  data.left_phase = 0;

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
  maybeDie( Pa_OpenStream(&stream, NULL, &outputParameters, SAMPLE_RATE, FRAMES_PER_BUFFER, paClipOff, patestCallback, &data ) );
  maybeDie( Pa_StartStream( stream ) );

  printf("Play for %d seconds.\n", NUM_SECONDS );
  Pa_Sleep( NUM_SECONDS * 1000 );

  maybeDie( Pa_StopStream( stream ) );
  maybeDie( Pa_CloseStream( stream ) );
  Pa_Terminate();
  
  return 0; }
