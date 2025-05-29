<script>
  import "../../style.css"; // Adjust path as needed based on your project structure
  import { goto } from '$app/navigation';
  import { fly } from 'svelte/transition';
  import { onMount, onDestroy } from 'svelte';
  import { writable, derived, get } from 'svelte/store';

  // --- Carousel State and Logic (now at top level of page script) ---
  const currentIndex = writable(0);
  const totalItems = writable(0);
  const itemsPerPage = writable(3); // Fixed to 3 for this request, used for md screens
  const contentRef = writable(null); // Store for the CarouselContent div
  const itemNodes = writable([]); // Store for item DOM nodes
  const itemsCurrentlyInView = writable(1); // <--- New writable store for dynamic view count

  // Local variables to bind directly to DOM elements
  let carouselContentDiv; // For the main carousel content div
  let carouselItemDivs = []; // Array for individual carousel item divs

  // Helper function to calculate how many items should be visible
  function calculateItemsInView() {
    // Assuming Tailwind's 'md' breakpoint is 768px. Adjust if your config differs.
    if (typeof window !== 'undefined' && window.innerWidth >= 768) {
      return get(itemsPerPage); // On larger screens, display 3 items
    }
    return 1; // On smaller screens, display 1 item
  }

  // Function to update the carousel's visual transform
  function updateCarouselTransform() {
    const currentContentRef = get(contentRef);
    const currentItemNodes = get(itemNodes);
    const currentCurrentIndex = get(currentIndex);

    const currentOrientation = 'vertical'; // Hardcoded as per previous context

    if (currentContentRef && currentItemNodes.length > 0) {
      const firstItemNode = currentItemNodes[0];
      if (firstItemNode) {
        let offset = 0;
        if (currentOrientation === 'vertical') {
          // Get the computed style to include margins in the item height calculation
          const itemComputedStyle = window.getComputedStyle(firstItemNode);
          const itemHeightWithMargins = firstItemNode.offsetHeight +
                                        parseFloat(itemComputedStyle.marginTop) +
                                        parseFloat(itemComputedStyle.marginBottom);

          offset = currentCurrentIndex * itemHeightWithMargins;
          currentContentRef.style.transform = `translateY(-${offset}px)`;
        } else {
          const itemWidth = firstItemNode.offsetWidth; // For horizontal, would use offsetWidth
          offset = currentCurrentIndex * itemWidth;
          currentContentRef.style.transform = `translateX(-${offset}px)`;
        }
        currentContentRef.style.transition = 'transform 0.3s ease-in-out';
      }
    }
  }

  // Functions for carousel navigation
  function nextCarouselItem() {
    currentIndex.update(n => {
      const currentTotalItems = get(totalItems);
      const currentItemsCurrentlyInView = get(itemsCurrentlyInView); // <--- Use store value
      // Max index is when the last visible group starts
      const maxIndex = Math.max(0, currentTotalItems - currentItemsCurrentlyInView);
      // Loop is false as per requirement
      return Math.min(maxIndex, n + 1);
    });
  }

  function previousCarouselItem() {
    currentIndex.update(n => {
      // Loop is false as per requirement
      return Math.max(0, n - 1);
    });
  }

  // Derived store to determine if the previous button should be disabled
  const isPreviousDisabled = derived([currentIndex], ([$currentIndex]) => {
    return $currentIndex === 0;
  });

  // Derived store to determine if the next button should be disabled
  const isNextDisabled = derived([currentIndex, totalItems, itemsCurrentlyInView], ([$currentIndex, $totalItems, $itemsCurrentlyInView]) => { // <--- Added itemsCurrentlyInView as dependency
    const maxIndex = Math.max(0, $totalItems - $itemsCurrentlyInView);
    return $currentIndex >= maxIndex;
  });

  // Lifecycle hooks to react to store changes and DOM readiness
  onMount(() => {
    // Set the store references after the component has mounted and elements are available
    contentRef.set(carouselContentDiv);
    // Ensure carouselItemDivs is a valid array of DOM nodes before setting
    itemNodes.set(carouselItemDivs.filter(Boolean)); // Filter out any undefined/null entries

    // Initialize total items and perform initial transform
    totalItems.set(get(itemNodes).length);
    itemsCurrentlyInView.set(calculateItemsInView()); // <--- Initialize on mount
    updateCarouselTransform();

    // Subscribe to changes in currentIndex, contentRef, and itemNodes
    const unsubscribeCurrentIndex = currentIndex.subscribe(() => {
      updateCarouselTransform();
    });
    const unsubscribeContentRef = contentRef.subscribe(() => {
      updateCarouselTransform();
    });
    const unsubscribeItemNodes = itemNodes.subscribe(() => {
      totalItems.set(get(itemNodes).length); // Update total items when itemNodes change
      // Adjust index to prevent being out of bounds if items are removed
      const currentItemsCurrentlyInView = get(itemsCurrentlyInView); // <--- Use store value
      currentIndex.update(n => Math.min(n, Math.max(0, get(totalItems) - currentItemsCurrentlyInView))); // Adjusted index
      updateCarouselTransform();
    });

    // Add resize listener to re-evaluate carousel state on screen size changes
    const handleResize = () => {
      const newItemsInView = calculateItemsInView();
      if (get(itemsCurrentlyInView) !== newItemsInView) { // Only update if value changed
        itemsCurrentlyInView.set(newItemsInView); // <--- Update the store
        // Re-calculate maxIndex and adjust currentIndex if needed
        currentIndex.update(n => Math.min(n, Math.max(0, get(totalItems) - newItemsInView)));
        updateCarouselTransform(); // Re-render carousel position
      }
    };
    window.addEventListener('resize', handleResize);


    return () => {
      // Unsubscribe on component destroy
      unsubscribeCurrentIndex();
      unsubscribeContentRef();
      unsubscribeItemNodes();
      window.removeEventListener('resize', handleResize); // Clean up resize listener
    };
  });

  onDestroy(() => {
    // Clear references on component destroy
    contentRef.set(null);
    itemNodes.set([]);
  });

  // --- Page Transition Logic ---
  const pageTransitionOptions = {
    duration: 500, // milliseconds
  };

  /**
   * Handles navigation back to the home page.
   */
  function goToHome() {
    goto('/');
  }

  // Array of items for the carousel
  const carouselItems = [
    'BrainRNA-Seq',
    'STRING',
    'iLINCS',
    'Lookup',
    'BrainCloud',
    'GTEx',
    'BrainAtlas',
    'GWAS',
    'IDG',
    'Report'
  ];
</script>

<div
  class="hero bg-base-neutral min-h-screen relative overflow-hidden flex items-center justify-center"
  in:fly|local="{{ x: window.innerWidth, ...pageTransitionOptions }}"
  out:fly|local="{{ x: window.innerWidth, ...pageTransitionOptions }}"
>
  <div class="hero-content text-center flex flex-col items-center justify-start gap-y-8">
    <button
      on:click={previousCarouselItem}
      disabled={$isPreviousDisabled}
      class="p-2 bg-gray-700 text-white rounded-full opacity-75 hover:opacity-100 disabled:opacity-30 disabled:cursor-not-allowed transition-opacity duration-200"
    >
      <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-chevron-up"><path d="m18 15-6-6-6 6"/></svg>
    </button>

    <div class="w-full max-w-xs mx-auto relative h-[116px] sm:h-[116px] md:h-[348px] overflow-hidden">
      <div bind:this={carouselContentDiv} class="flex flex-col h-full -mt-1">
        {#each carouselItems as item, i (item)}
          <div bind:this={carouselItemDivs[i]} class="flex-shrink-0 w-full basis-full md:basis-1/3">
            <div class="my-2">
              <div class="card w-full bg-base-100 card-xs shadow-sm">
                <div class="card-body flex items-center justify-center p-6 bg-base-200 rounded-lg shadow-md">
                  <span class="text-lg font-semibold text-primary text-center">{item}</span>
                </div>
              </div>
            </div>
          </div>
        {/each}
      </div>
    </div>

    <button
      on:click={nextCarouselItem}
      disabled={$isNextDisabled}
      class="p-2 bg-gray-700 text-white rounded-full opacity-75 hover:opacity-100 disabled:opacity-30 disabled:cursor-not-allowed transition-opacity duration-200"
    >
      <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-chevron-down"><path d="m6 9 6 6 6-6"/></svg>
    </button>

    <button class="btn btn-secondary mt-8" on:click={goToHome}>Go Back Home</button>
  </div>

  <button
    class="absolute left-8 top-1/2 -translate-y-1/2
           bg-secondary text-secondary-content p-4 rounded-full shadow-lg
           hover:scale-105 transition-transform duration-200 ease-out
           flex items-center justify-center z-10"
    aria-label="Go back to Home"
    on:click={goToHome}
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
      <path d="M19 12H5M12 19l-7-7 7-7" />
    </svg>
  </button>
</div>

