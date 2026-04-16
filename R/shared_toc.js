$(function() {
  var tocEl = $('<div id="sticky-toc"></div>').appendTo('body');
  var tocBtn = $('<button id="toc-mobile-btn">\u2630</button>').appendTo('body');
  var tocOverlay = $('<div id="toc-overlay"></div>').appendTo('body');

  tocBtn.on('click', function() {
    tocEl.toggleClass('toc-open');
    tocOverlay.toggleClass('toc-open');
  });
  tocOverlay.on('click', function() {
    tocEl.removeClass('toc-open');
    tocOverlay.removeClass('toc-open');
  });

  function buildToc() {
    var activeTab = $('.tab-pane.active');
    if (!activeTab.length) return;
    var sections = activeTab.find('.section-title');
    if (sections.length < 2) { tocEl.hide(); return; }

    var html = '<div class="toc-title">Spis tre\u015bci</div>';
    sections.each(function(i) {
      var el = $(this);
      var id = 'toc-sec-' + i;
      el.attr('id', id);
      var text = el.text().trim();
      if (text.length > 35) text = text.substring(0, 33) + '...';
      html += '<a href="#' + id + '" data-idx="' + i + '">' + text + '</a>';
    });
    tocEl.html(html).show();
  }

  function updateActive() {
    var scrollTop = $(window).scrollTop();
    var current = null;
    $('.tab-pane.active .section-title').each(function() {
      if ($(this).offset().top - 100 <= scrollTop) current = $(this).attr('id');
    });
    tocEl.find('a').removeClass('toc-active');
    if (current) tocEl.find('a[href="#' + current + '"]').addClass('toc-active');
  }

  tocEl.on('click', 'a', function(e) {
    e.preventDefault();
    var target = $($(this).attr('href'));
    if (target.length) {
      $('html, body').animate({ scrollTop: target.offset().top - 60 }, 300);
    }
    tocEl.removeClass('toc-open');
    tocOverlay.removeClass('toc-open');
  });

  $(document).on('shown.bs.tab', function() { setTimeout(buildToc, 150); });
  $(window).on('scroll', updateActive);
  setTimeout(buildToc, 500);
});
