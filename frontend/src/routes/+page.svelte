<script>
  import "../style.css";
  import { goto } from '$app/navigation'; // Import goto for programmatic navigation
  import { fly } from 'svelte/transition'; // Changed from 'slide' to 'fly'

  // Transition options for the page itself
  const pageTransitionOptions = {
    duration: 500, // milliseconds
    // 'x' will be used by 'fly' to control horizontal movement
    // 'opacity' is also animated by 'fly' by default
  };

  /**
   * Handles navigation to the app page.
   */
  function goToApp() {
    goto('/app');
  }
</script>

<div
  class="hero bg-base-200 min-h-screen relative overflow-hidden"
  in:fly|local="{{ x: -window.innerWidth, ...pageTransitionOptions }}"  out:fly|local="{{ x: -window.innerWidth, ...pageTransitionOptions }}" >
  <div class="hero-content text-center">
    <div class="max-w-md align-center">
      <img alt="Kaleidoscope Logo" class="size-50 self-center mx-auto" src="/kaleidoscope-logo.svg"/>
      <h1 class="text-5xl font-bold">Kaleidoscope 2.0</h1>
      <p class="py-6">
        In-Silico Data Exploration of Biological Datasets
      </p>
      <label class="input m-8 flex items-center gap-2">
        <svg class="h-[1em] opacity-50" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
          <g
            stroke-linejoin="round"
            stroke-linecap="round"
            stroke-width="2.5"
            fill="none"
            stroke="currentColor"
          >
            <circle cx="11" cy="11" r="8"></circle>
            <path d="m21 21-4.3-4.3"></path>
          </g>
        </svg>
        <input type="search" class="grow" placeholder="Search" />
      </label>
      <button class="btn btn-primary mt-4" on:click={goToApp}>Get Started</button>
    </div>
  </div>

  <button
    class="absolute right-8 top-1/2 -translate-y-1/2
           bg-primary text-primary-content p-4 rounded-full shadow-lg
           hover:scale-105 transition-transform duration-200 ease-out
           flex items-center justify-center z-10"
    aria-label="Go to App"
    on:click={goToApp}
  >
    <svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      stroke-width="2"
      stroke-linecap="round"
      stroke-linejoin="round"
      class="w-8 h-8"
    >
      <path d="M5 12h14M12 5l7 7-7 7" />
    </svg>
  </button>
</div>

