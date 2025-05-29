<script>
  import { slide } from 'svelte/transition';
  import { quintOut } from 'svelte/easing'; // You can choose a different easing function
</script>

<div
  class="app-container"
  in:slide="{{ x: window.innerWidth, duration: 500, easing: quintOut }}"
  out:slide="{{ x: -window.innerWidth, duration: 500, easing: quintOut }}"
>
  <slot />
</div>

<style>
  /* Optional: You might need to set overflow-x: hidden on a parent element
     to prevent horizontal scrollbars during the transition, but SvelteKit
     often handles this well with its routing. */
  .app-container {
    width: 100%;
    height: 100%;
    position: absolute; /* or relative, depending on your layout */
    top: 0;
    left: 0;
  }
</style>
