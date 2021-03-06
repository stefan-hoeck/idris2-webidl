// Extracted from https://www.w3.org/TR/web-animations-1/

[Exposed=Window]
interface AnimationTimeline {
    readonly attribute double? currentTime;
};

dictionary DocumentTimelineOptions {
  DOMHighResTimeStamp originTime = 0;
};

[Exposed=Window,
 Constructor (optional DocumentTimelineOptions options)]
interface DocumentTimeline : AnimationTimeline {
};

[Exposed=Window,
 Constructor (optional AnimationEffect? effect = null,
              optional AnimationTimeline? timeline)]
interface Animation : EventTarget {
             attribute DOMString                id;
             attribute AnimationEffect?         effect;
             attribute AnimationTimeline?       timeline;
             attribute double?                  startTime;
             attribute double?                  currentTime;
             attribute double                   playbackRate;
    readonly attribute AnimationPlayState       playState;
    readonly attribute boolean                  pending;
    readonly attribute Promise<Animation>       ready;
    readonly attribute Promise<Animation>       finished;
             attribute EventHandler             onfinish;
             attribute EventHandler             oncancel;
    void cancel ();
    void finish ();
    void play ();
    void pause ();
    void updatePlaybackRate (double playbackRate);
    void reverse ();
};

enum AnimationPlayState { "idle", "running", "paused", "finished" };

[Exposed=Window]
interface AnimationEffect {
    EffectTiming         getTiming();
    ComputedEffectTiming getComputedTiming();
    void                 updateTiming(optional OptionalEffectTiming timing);
};

dictionary EffectTiming {
    double                             delay = 0;
    double                             endDelay = 0;
    FillMode                           fill = "auto";
    double                             iterationStart = 0.0;
    unrestricted double                iterations = 1.0;
    (unrestricted double or DOMString) duration = "auto";
    PlaybackDirection                  direction = "normal";
    DOMString                          easing = "linear";
};

dictionary OptionalEffectTiming {
    double                             delay;
    double                             endDelay;
    FillMode                           fill;
    double                             iterationStart;
    unrestricted double                iterations;
    (unrestricted double or DOMString) duration;
    PlaybackDirection                  direction;
    DOMString                          easing;
};

enum FillMode { "none", "forwards", "backwards", "both", "auto" };

enum PlaybackDirection { "normal", "reverse", "alternate", "alternate-reverse" };

dictionary ComputedEffectTiming : EffectTiming {
    unrestricted double  endTime;
    unrestricted double  activeDuration;
    double?              localTime;
    double?              progress;
    unrestricted double? currentIteration;
};

[Exposed=Window,
 Constructor ((Element or CSSPseudoElement)? target,
              object? keyframes,
              optional (unrestricted double or KeyframeEffectOptions) options),
 Constructor (KeyframeEffect source)]
interface KeyframeEffect : AnimationEffect {
    attribute (Element or CSSPseudoElement)? target;
    attribute IterationCompositeOperation    iterationComposite;
    attribute CompositeOperation             composite;
    sequence<object> getKeyframes ();
    void             setKeyframes (object? keyframes);
};

dictionary BaseComputedKeyframe {
     double?                  offset = null;
     double                   computedOffset;
     DOMString                easing = "linear";
     CompositeOperationOrAuto composite = "auto";
};

dictionary BasePropertyIndexedKeyframe {
    (double? or sequence<double?>)                         offset = [];
    (DOMString or sequence<DOMString>)                     easing = [];
    (CompositeOperationOrAuto or sequence<CompositeOperationOrAuto>) composite = [];
};

dictionary BaseKeyframe {
    double?                  offset = null;
    DOMString                easing = "linear";
    CompositeOperationOrAuto composite = "auto";
};

dictionary KeyframeEffectOptions : EffectTiming {
    IterationCompositeOperation iterationComposite = "replace";
    CompositeOperation          composite = "replace";
};

enum IterationCompositeOperation {"replace", "accumulate"};

enum CompositeOperation {"replace", "add", "accumulate"};

enum CompositeOperationOrAuto {"replace", "add", "accumulate", "auto"};

interface mixin Animatable {
    Animation           animate (object? keyframes,
                                 optional (unrestricted double or KeyframeAnimationOptions) options);
    sequence<Animation> getAnimations ();
};
dictionary KeyframeAnimationOptions : KeyframeEffectOptions {
    DOMString id = "";
};

partial interface Document {
    readonly attribute DocumentTimeline timeline;
    sequence<Animation> getAnimations();
};

Element includes Animatable;

CSSPseudoElement includes Animatable;

[Exposed=Window,
 Constructor (DOMString type, optional AnimationPlaybackEventInit eventInitDict)]
interface AnimationPlaybackEvent : Event {
    readonly attribute double? currentTime;
    readonly attribute double? timelineTime;
};
dictionary AnimationPlaybackEventInit : EventInit {
    double? currentTime = null;
    double? timelineTime = null;
};
